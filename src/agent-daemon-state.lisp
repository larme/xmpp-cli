(in-package #:xmpp-cli/agent-daemon)

(defstruct daemon-state
  backend
  profile-name
  profile
  agent-config
  control
  server-socket
  token
  (stop-p nil)
  (xmpp-status :starting)
  connection
  connected-at
  last-error
  (lock (bt:make-lock "xmpp-cli agent daemon"))
  (xmpp-write-lock (bt:make-lock "xmpp-cli XMPP writer"))
  (pending-iqs (make-hash-table :test 'equal))
  (pending-iq-counter 0)
  (pending-room-joins (make-hash-table :test 'equal))
  (pending-permissions (make-hash-table :test 'equal))
  (pending-permission-counter 0)
  (room-occupants (make-hash-table :test 'equal)))

(defstruct pending-request
  id
  response
  error-text
  (condition (bt:make-condition-variable :name "xmpp-cli pending request")))

(defstruct pending-permission
  id
  route-id
  route-code
  fallback-to
  target-kind
  target
  room-jid
  created-at
  expires-at
  response
  error-text
  (condition (bt:make-condition-variable :name "xmpp-cli pending permission")))

(defparameter *reply-whitespace* '(#\Space #\Tab #\Newline #\Return))
(defparameter *daemon-thread-stop-wait-seconds* 5)
(defparameter *daemon-client-timeout-seconds* 2)
(defparameter *default-iq-timeout-seconds* 10)
(defparameter *default-room-join-timeout-seconds* 10)

(defun notify-pending-request (pending)
  (ignore-errors
    (bt:condition-notify (pending-request-condition pending))))

(defun pending-table (state table-reader)
  (funcall table-reader state))

(defun fail-pending-table-locked (state table-reader error-text)
  (let ((table (pending-table state table-reader)))
    (maphash (lambda (id pending)
               (declare (ignore id))
               (setf (pending-request-error-text pending) error-text)
               (notify-pending-request pending))
             table)
    (clrhash table)))

(defun fail-pending-iqs-locked (state error-text)
  (fail-pending-table-locked state #'daemon-state-pending-iqs error-text))

(defun fail-pending-room-joins-locked (state error-text)
  (fail-pending-table-locked state #'daemon-state-pending-room-joins error-text))

(defun notify-pending-permission (pending)
  (ignore-errors
    (bt:condition-notify (pending-permission-condition pending))))

(defun fail-pending-permissions-locked (state error-text)
  (let ((table (daemon-state-pending-permissions state)))
    (maphash (lambda (id pending)
               (declare (ignore id))
               (setf (pending-permission-error-text pending) error-text)
               (notify-pending-permission pending))
             table)
    (clrhash table)))

(defun state-stopped-p (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (daemon-state-stop-p state)))

(defun wake-control-server (server)
  (let ((port (and server
                   (ignore-errors
                     (usocket:get-local-port server)))))
    (when port
      (ignore-errors
        (let ((client (usocket:socket-connect "127.0.0.1"
                                              port
                                              :element-type '(unsigned-byte 8)
                                              :timeout 1)))
          (usocket:socket-close client))))))

(defun request-stop (state)
  (let ((connection nil)
        (server nil))
    (bt:with-lock-held ((daemon-state-lock state))
      (setf (daemon-state-stop-p state) t)
      (setf connection (daemon-state-connection state))
      (setf server (daemon-state-server-socket state))
      (fail-pending-iqs-locked state "XMPP daemon is stopping.")
      (fail-pending-room-joins-locked state "XMPP daemon is stopping.")
      (fail-pending-permissions-locked state "XMPP daemon is stopping."))
    (when server
      (wake-control-server server)
      (ignore-errors
        (usocket:socket-close server)))
    (when connection
      (ignore-errors
        (close-connection (daemon-state-backend state) connection)))))

(defun mark-connecting (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (daemon-state-xmpp-status state) :connecting)
    (setf (daemon-state-connection state) nil)
    (setf (daemon-state-connected-at state) nil)
    (clrhash (daemon-state-room-occupants state))))

(defun mark-connected (state connection)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (daemon-state-xmpp-status state) :connected)
    (setf (daemon-state-connection state) connection)
    (setf (daemon-state-connected-at state) (now-iso8601))
    (setf (daemon-state-last-error state) nil)))

(defun mark-disconnected (state error-text)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (daemon-state-xmpp-status state)
          (if (daemon-state-stop-p state) :stopped :disconnected))
    (setf (daemon-state-connection state) nil)
    (setf (daemon-state-connected-at state) nil)
    (setf (daemon-state-last-error state) error-text)
    (clrhash (daemon-state-room-occupants state))
    (fail-pending-iqs-locked state (or error-text "XMPP connection closed."))
    (fail-pending-room-joins-locked state (or error-text "XMPP connection closed."))
    (fail-pending-permissions-locked state (or error-text "XMPP connection closed."))))

(defun route-ttl-days (state)
  (getf (daemon-state-agent-config state) :route-ttl-days))

(defun daemon-send-with-connection (state sender &key fail-room-joins-p)
  (let ((backend (daemon-state-backend state))
        (connection nil))
    (bt:with-lock-held ((daemon-state-lock state))
      (unless (and (eq (daemon-state-xmpp-status state) :connected)
                   (daemon-state-connection state))
        (return-from daemon-send-with-connection
          (values nil "XMPP daemon connection is not ready.")))
      (setf connection (daemon-state-connection state)))
    (handler-case
        (progn
          (call-with-xmpp-write-lock
           state
           (lambda ()
             (funcall sender backend connection)))
          (values t nil))
      (error (condition)
        (let ((text (princ-to-string condition)))
          (bt:with-lock-held ((daemon-state-lock state))
            (setf (daemon-state-xmpp-status state) :disconnected)
            (setf (daemon-state-last-error state) text)
            (setf (daemon-state-connection state) nil)
            (fail-pending-iqs-locked state text)
            (when fail-room-joins-p
              (fail-pending-room-joins-locked state text))
            (fail-pending-permissions-locked state text))
          (values nil text))))))

(defun next-permission-id (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (incf (daemon-state-pending-permission-counter state))
    (format nil "permission-~36r-~36r"
            (get-universal-time)
            (daemon-state-pending-permission-counter state))))

(defun permission-expired-p (pending &optional (now (get-universal-time)))
  (let ((expires-at (pending-permission-expires-at pending)))
    (and expires-at (>= now expires-at))))

(defun prune-expired-permissions-locked (state)
  (let ((now (get-universal-time))
        (table (daemon-state-pending-permissions state))
        (expired nil))
    (maphash (lambda (id pending)
               (when (permission-expired-p pending now)
                 (push id expired)
                 (setf (pending-permission-error-text pending)
                       "Permission request expired.")
                 (notify-pending-permission pending)))
             table)
    (dolist (id expired)
      (remhash id table))))

(defun register-pending-permission (state route timeout-seconds fallback-to)
  (let* ((id (next-permission-id state))
         (now (get-universal-time))
         (pending (make-pending-permission
                   :id id
                   :route-id (getf route :route-id)
                   :route-code (getf route :code)
                   :fallback-to fallback-to
                   :created-at now
                   :expires-at (+ now timeout-seconds))))
    (bt:with-lock-held ((daemon-state-lock state))
      (setf (gethash id (daemon-state-pending-permissions state))
            pending))
    pending))

(defun remove-pending-permission (state pending)
  (bt:with-lock-held ((daemon-state-lock state))
    (remhash (pending-permission-id pending)
             (daemon-state-pending-permissions state))))

(defun update-pending-permission-target (state pending response)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (pending-permission-target-kind pending)
          (getf response :target-kind))
    (setf (pending-permission-target pending)
          (getf response :target))
    (setf (pending-permission-room-jid pending)
          (or (getf response :room)
              (and (eq (getf response :target-kind) :room)
                   (getf response :target))))))

(defun answer-pending-permission (state pending decision &key message)
  (bt:with-lock-held ((daemon-state-lock state))
    (let ((current (gethash (pending-permission-id pending)
                            (daemon-state-pending-permissions state))))
      (when current
        (setf (pending-permission-response current)
              (list :decision decision :message message))
        (remhash (pending-permission-id current)
                 (daemon-state-pending-permissions state))
        (notify-pending-permission current)
        t))))

(defun latest-pending-permission-for-route (state route-id)
  (bt:with-lock-held ((daemon-state-lock state))
    (prune-expired-permissions-locked state)
    (let ((latest nil))
      (maphash
       (lambda (id pending)
         (declare (ignore id))
         (when (and (string= route-id
                             (or (pending-permission-route-id pending) ""))
                    (or (null latest)
                        (> (pending-permission-created-at pending)
                           (pending-permission-created-at latest))))
           (setf latest pending)))
       (daemon-state-pending-permissions state))
      latest)))

(defun wait-for-pending-permission (state pending timeout-seconds)
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds
                               internal-time-units-per-second)))))
    (bt:with-lock-held ((daemon-state-lock state))
      (loop
        (when (pending-permission-response pending)
          (return (pending-permission-response pending)))
        (when (pending-permission-error-text pending)
          (error "~a" (pending-permission-error-text pending)))
        (let ((remaining (/ (- deadline (get-internal-real-time))
                            internal-time-units-per-second)))
          (when (<= remaining 0)
            (remhash (pending-permission-id pending)
                     (daemon-state-pending-permissions state))
            (error "Timed out waiting for XMPP permission reply."))
          (bt:condition-wait (pending-permission-condition pending)
                             (daemon-state-lock state)
                             :timeout remaining))))))

(defun daemon-send-text (state to body)
  (daemon-send-with-connection
   state
   (lambda (backend connection)
     (send-connected-text backend connection to body))))

(defun daemon-send-room-text (state room-jid body)
  (daemon-send-with-connection
   state
   (lambda (backend connection)
     (send-room-message backend connection room-jid body))
   :fail-room-joins-p t))

(defun call-with-xmpp-write-lock (state thunk)
  (bt:with-lock-held ((daemon-state-xmpp-write-lock state))
    (funcall thunk)))

(defun next-iq-id (state &optional (prefix "xmppcli"))
  (bt:with-lock-held ((daemon-state-lock state))
    (incf (daemon-state-pending-iq-counter state))
    (format nil "~a-~36r-~36r"
            prefix
            (get-universal-time)
            (daemon-state-pending-iq-counter state))))

(defun register-pending-request (state table-reader id duplicate-label)
  (let ((pending (make-pending-request :id id)))
    (bt:with-lock-held ((daemon-state-lock state))
      (let ((table (pending-table state table-reader)))
        (when (gethash id table)
          (error "Duplicate ~a: ~a" duplicate-label id))
        (setf (gethash id table) pending)))
    pending))

(defun remove-pending-request (state table-reader id)
  (bt:with-lock-held ((daemon-state-lock state))
    (remhash id (pending-table state table-reader))))

(defun resolve-pending-request (state table-reader id response)
  (when (and id (stringp id))
    (bt:with-lock-held ((daemon-state-lock state))
      (let* ((table (pending-table state table-reader))
             (pending (gethash id table)))
        (when pending
          (setf (pending-request-response pending) response)
          (remhash id table)
          (notify-pending-request pending)
          t)))))

(defun wait-for-pending-request (state
                                 table-reader
                                 pending
                                 timeout-seconds
                                 failure-label
                                 timeout-label)
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds
                               internal-time-units-per-second)))))
    (bt:with-lock-held ((daemon-state-lock state))
      (loop
        (when (pending-request-response pending)
          (return (pending-request-response pending)))
        (when (pending-request-error-text pending)
          (error "~a ~a failed: ~a"
                 failure-label
                 (pending-request-id pending)
                 (pending-request-error-text pending)))
        (let ((remaining (/ (- deadline (get-internal-real-time))
                            internal-time-units-per-second)))
          (when (<= remaining 0)
            (remhash (pending-request-id pending)
                     (pending-table state table-reader))
            (error "~a ~a." timeout-label (pending-request-id pending)))
          (bt:condition-wait (pending-request-condition pending)
                             (daemon-state-lock state)
                             :timeout remaining))))))

(defun register-pending-iq (state id)
  (register-pending-request state
                            #'daemon-state-pending-iqs
                            id
                            "pending IQ id"))

(defun remove-pending-iq (state id)
  (remove-pending-request state #'daemon-state-pending-iqs id))

(defun resolve-pending-iq (state stanza)
  (resolve-pending-request state
                           #'daemon-state-pending-iqs
                           (getf stanza :id)
                           stanza))

(defun wait-for-pending-iq (state pending timeout-seconds)
  (wait-for-pending-request state
                            #'daemon-state-pending-iqs
                            pending
                            timeout-seconds
                            "IQ request"
                            "Timed out waiting for IQ response"))

(defun register-pending-room-join (state room-full-jid)
  (register-pending-request state
                            #'daemon-state-pending-room-joins
                            room-full-jid
                            "pending room join"))

(defun remove-pending-room-join (state room-full-jid)
  (remove-pending-request state
                          #'daemon-state-pending-room-joins
                          room-full-jid))

(defun resolve-pending-room-join (state stanza)
  (resolve-pending-request state
                           #'daemon-state-pending-room-joins
                           (getf stanza :from)
                           stanza))

(defun wait-for-pending-room-join (state pending timeout-seconds)
  (wait-for-pending-request state
                            #'daemon-state-pending-room-joins
                            pending
                            timeout-seconds
                            "Room join"
                            "Timed out waiting for room join presence"))

(defun current-xmpp-connection-or-error (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (unless (and (eq (daemon-state-xmpp-status state) :connected)
                 (daemon-state-connection state))
      (error "XMPP daemon connection is not ready."))
    (daemon-state-connection state)))

(defun send-iq-and-wait (state sender &key id
                                      (timeout-seconds
                                       *default-iq-timeout-seconds*))
  "Call SENDER with CONNECTION and IQ id, then wait for the matching IQ reply."
  (let* ((iq-id (or id (next-iq-id state)))
         (pending (register-pending-iq state iq-id))
         (sent nil))
    (unwind-protect
         (let ((connection (current-xmpp-connection-or-error state)))
           (call-with-xmpp-write-lock
            state
            (lambda ()
              (funcall sender connection iq-id)
              (setf sent t)))
           (wait-for-pending-iq state pending timeout-seconds))
      (unless (pending-request-response pending)
        (remove-pending-iq state iq-id))
      (unless sent
        (setf (pending-request-error-text pending)
              "IQ request was not sent.")))))

(defun join-room-and-wait (state room-full-jid &key
                                               (timeout-seconds
                                                *default-room-join-timeout-seconds*))
  (let ((pending (register-pending-room-join state room-full-jid))
        (sent nil))
    (unwind-protect
         (let ((connection (current-xmpp-connection-or-error state))
               (backend (daemon-state-backend state)))
           (call-with-xmpp-write-lock
            state
            (lambda ()
              (join-room backend connection room-full-jid)
              (setf sent t)))
           (wait-for-pending-room-join state pending timeout-seconds))
      (unless (pending-request-response pending)
        (remove-pending-room-join state room-full-jid))
      (unless sent
        (setf (pending-request-error-text pending)
              "Room join request was not sent.")))))

(defun normalized-room-key (room-jid)
  (and room-jid (string-downcase room-jid)))

(defun room-occupants-table (state room-jid &key create)
  (let ((key (normalized-room-key room-jid)))
    (when key
      (or (gethash key (daemon-state-room-occupants state))
          (and create
               (setf (gethash key (daemon-state-room-occupants state))
                     (make-hash-table :test 'equal)))))))

(defun remember-room-occupant (state stanza)
  (let ((room-jid (getf stanza :room-jid))
        (nick (getf stanza :room-nick)))
    (when (and room-jid nick (getf stanza :muc-user-p))
      (bt:with-lock-held ((daemon-state-lock state))
        (let ((occupants (room-occupants-table state room-jid :create t)))
          (if (string-equal "unavailable" (or (getf stanza :type) ""))
              (remhash nick occupants)
              (setf (gethash nick occupants)
                    (list :room-jid room-jid
                          :nick nick
                          :jid (getf stanza :muc-jid)
                          :affiliation (getf stanza :muc-affiliation)
                          :role (getf stanza :muc-role)
                          :seen-at (now-iso8601)))))))))

(defun room-occupant (state room-jid nick)
  (bt:with-lock-held ((daemon-state-lock state))
    (let ((occupants (room-occupants-table state room-jid)))
      (and occupants (gethash nick occupants)))))
