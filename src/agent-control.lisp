(in-package #:xmpp-cli/agent-daemon)

(defun route-count (state)
  (handler-case
      (length (load-active-routes :route-ttl-days (route-ttl-days state)))
    (error ()
      nil)))

(defun status-plist (state)
  (let ((routes (route-count state)))
    (bt:with-lock-held ((daemon-state-lock state))
      (let ((control (daemon-state-control state)))
        (list :ok t
              :pid (current-process-id)
              :profile (daemon-state-profile-name state)
              :control-host (getf control :host)
              :control-port (getf control :port)
              :profile-jid (getf control :profile-jid)
              :profile-digest (getf control :profile-digest)
              :xmpp-status (daemon-state-xmpp-status state)
              :connected (eq (daemon-state-xmpp-status state) :connected)
              :connected-at (daemon-state-connected-at state)
              :last-error (daemon-state-last-error state)
              :route-count routes)))))

(defun state-account-domain (state)
  (or (getf (daemon-state-profile state) :domain)
      (let ((jid (getf (daemon-state-profile state) :jid)))
        (and (stringp jid)
             (let ((at (position #\@ jid)))
               (and at
                    (subseq jid (1+ at))))))))

(defun daemon-disco-items (state to)
  (let* ((backend (daemon-state-backend state))
         (response
           (send-iq-and-wait
            state
            (lambda (connection id)
              (send-disco-items backend connection to id))
            :timeout-seconds *default-iq-timeout-seconds*)))
    (getf (ensure-iq-ok response "disco#items" to) :disco-items)))

(defun daemon-disco-info (state to)
  (let* ((backend (daemon-state-backend state))
         (response
           (send-iq-and-wait
            state
            (lambda (connection id)
              (send-disco-info backend connection to id))
            :timeout-seconds *default-iq-timeout-seconds*))
         (result (ensure-iq-ok response "disco#info" to)))
    (list :identities (getf result :disco-identities)
          :features (getf result :disco-features))))

(defun daemon-discover-muc-service (state &key force)
  (let ((domain (state-account-domain state)))
    (unless (and domain (plusp (length domain)))
      (error "Current profile does not include an account domain."))
    (resolve-muc-service domain
                         (daemon-state-agent-config state)
                         (lambda (to)
                           (daemon-disco-items state to))
                         (lambda (to)
                           (daemon-disco-info state to))
                         :force force)))

(defun matching-profile-digest-p (state expected-profile-digest)
  (let ((actual-profile-digest
          (getf (daemon-state-control state) :profile-digest)))
    (and (stringp expected-profile-digest)
         (stringp actual-profile-digest)
         (string= expected-profile-digest actual-profile-digest))))

(defun daemon-notify-route (state route-id fallback-to body)
  (let ((room (and (stringp route-id)
                   (plusp (length route-id))
                   (find-active-room-by-route-id route-id))))
    (cond
      (room
       (let ((room-jid (getf room :room-jid)))
         (cond
           ((and (stringp fallback-to)
                 (plusp (length fallback-to))
                 (not (room-has-known-allowed-occupant-p state room)))
            (multiple-value-bind (ok error-text)
                (daemon-send-text state fallback-to body)
              (if ok
                  (list :ok t
                        :target-kind :jid
                        :target fallback-to
                        :room room-jid
                        :route-id route-id)
                  (list :ok nil
                        :error error-text
                        :target-kind :jid
                        :target fallback-to
                        :room room-jid
                        :route-id route-id
                        :fallback-allowed t))))
           (t
            (multiple-value-bind (ok error-text)
                (daemon-send-room-text state room-jid body)
              (if ok
                  (progn
                    (ignore-errors
                      (mark-room-activity room))
                    (list :ok t
                          :target-kind :room
                          :target room-jid
                          :route-id route-id))
                  (list :ok nil
                        :error error-text
                        :target-kind :room
                        :target room-jid
                        :route-id route-id
                        :fallback-allowed (and (stringp fallback-to)
                                               (plusp (length fallback-to))))))))))
      ((and (stringp fallback-to) (plusp (length fallback-to)))
       (multiple-value-bind (ok error-text)
           (daemon-send-text state fallback-to body)
         (if ok
             (list :ok t
                   :target-kind :jid
                   :target fallback-to
                   :route-id route-id)
             (list :ok nil
                   :error error-text
                   :target-kind :jid
                   :target fallback-to
                   :route-id route-id
                   :fallback-allowed t))))
      (t
       (list :ok nil
             :error "No room is bound to this route and no direct fallback JID was provided."
             :route-id route-id
             :fallback-allowed nil)))))

(defun handle-send-request (state request)
  (let ((to (getf request :to))
        (body (getf request :body))
        (expected-profile-digest
          (getf request :expected-profile-digest)))
    (cond
      ((not (matching-profile-digest-p state expected-profile-digest))
       (list :ok nil
             :error "Daemon profile does not match send request."))
      ((and (stringp to) (plusp (length to)) (stringp body))
       (multiple-value-bind (ok error-text) (daemon-send-text state to body)
         (if ok
             (list :ok t)
             (list :ok nil :error error-text))))
      (t
       (list :ok nil :error "Malformed send request.")))))

(defun handle-notify-request (state request)
  (let ((route-id (getf request :route-id))
        (fallback-to (getf request :fallback-to))
        (body (getf request :body))
        (expected-profile-digest
          (getf request :expected-profile-digest)))
    (cond
      ((not (matching-profile-digest-p state expected-profile-digest))
       (list :ok nil
             :error "Daemon profile does not match notify request."
             :fallback-allowed t))
      ((not (stringp body))
       (list :ok nil
             :error "Malformed notify request."
             :fallback-allowed nil))
      (t
       (daemon-notify-route state route-id fallback-to body)))))

(defun permission-request-timeout (request)
  (let ((timeout (getf request :timeout-seconds)))
    (if (and (integerp timeout) (plusp timeout))
        timeout
        300)))

(defun permission-request-bodies (request)
  (let ((bodies (getf request :bodies)))
    (cond
      ((and (listp bodies)
            (every #'stringp bodies)
            bodies)
       bodies)
      ((stringp (getf request :body))
       (list (getf request :body)))
      (t nil))))

(defun send-permission-request-bodies (state route-id fallback-to bodies pending)
  (let ((last-response nil))
    (dolist (body bodies (values t last-response nil))
      (let ((response (daemon-notify-route state route-id fallback-to body)))
        (setf last-response response)
        (when (getf response :ok)
          (update-pending-permission-target state pending response))
        (unless (getf response :ok)
          (return (values nil
                          response
                          (or (getf response :error)
                              "permission request notification failed"))))))))

(defun handle-permission-request (state request)
  (let* ((route-id (getf request :route-id))
         (fallback-to (getf request :fallback-to))
         (bodies (permission-request-bodies request))
         (timeout-seconds (permission-request-timeout request))
         (expected-profile-digest
           (getf request :expected-profile-digest))
         (route (and (stringp route-id)
                     (find-route-by-id
                      (active-routes-for-state state)
                      route-id)))
         (pending nil))
    (cond
      ((not (matching-profile-digest-p state expected-profile-digest))
       (list :ok nil
             :error "Daemon profile does not match permission request."
             :sent nil
             :fallback-allowed t))
      ((null route)
       (list :ok nil
             :error "Permission request route is unknown or stale."
             :sent nil
             :fallback-allowed t))
      ((null bodies)
       (list :ok nil
             :error "Malformed permission request."
             :sent nil
             :fallback-allowed nil))
      (t
       (unwind-protect
            (progn
              (setf pending
                    (register-pending-permission state
                                                 route
                                                 timeout-seconds
                                                 fallback-to))
              (multiple-value-bind (sent-ok send-response send-error)
                  (send-permission-request-bodies state
                                                  route-id
                                                  fallback-to
                                                  bodies
                                                  pending)
                (unless sent-ok
                  (remove-pending-permission state pending)
                  (return-from handle-permission-request
                    (list :ok nil
                          :error send-error
                          :sent nil
                          :fallback-allowed
                          (and (listp send-response)
                               (getf send-response :fallback-allowed)))))
                (handler-case
                    (let ((answer (wait-for-pending-permission
                                   state
                                   pending
                                   timeout-seconds)))
                      (setf pending nil)
                      (list :ok t
                            :sent t
                            :decision (getf answer :decision)
                            :message (getf answer :message)
                            :target-kind
                            (and (listp send-response)
                                 (getf send-response :target-kind))
                            :target
                            (and (listp send-response)
                                 (getf send-response :target))))
                  (error (condition)
                    (setf pending nil)
                    (list :ok nil
                          :sent t
                          :error (princ-to-string condition)
                          :fallback-allowed nil)))))
         (when pending
           (remove-pending-permission state pending)))))))

(defun handle-control-request (state request)
  (cond
    ((not (listp request))
     (list :ok nil :error "Malformed daemon request."))
    ((not (equal (getf request :token) (daemon-state-token state)))
     (list :ok nil :error "Unauthorized daemon request."))
    (t
     (case (getf request :op)
       (:status
        (status-plist state))
       (:send
        (handle-send-request state request))
       (:notify
        (handle-notify-request state request))
       (:permission-request
        (handle-permission-request state request))
       (:stop
        (request-stop state)
        (list :ok t))
       (:discover-muc
        (handler-case
            (append (list :ok t)
                    (daemon-discover-muc-service
                     state
                     :force (getf request :force)))
          (error (condition)
            (list :ok nil
                  :error (format nil "MUC discovery failed: ~a" condition)))))
       (otherwise
        (list :ok nil :error "Unknown daemon operation."))))))

(defun handle-client (state socket)
  (when socket
    (let ((stream nil))
      (unwind-protect
           (handler-case
               (progn
                 (setf stream (make-ipc-stream socket))
                 (let* ((request (read-ipc-message-with-timeout
                                  socket
                                  stream
                                  *daemon-client-timeout-seconds*
                                  "Daemon IPC request"))
                        (response (handle-control-request state request)))
                   (write-ipc-message stream response)))
             (error (condition)
               (when stream
                 (ignore-errors
                   (write-ipc-message
                    stream
                    (list :ok nil
                          :error (format nil "daemon request failed: ~a" condition)))))))
        (ignore-errors
          (usocket:socket-close socket))))))

(defun start-control-client-thread (state client)
  (bt:make-thread (lambda ()
                    (handle-client state client))
                  :name "xmpp-cli daemon control client"))

(defun accept-control-loop (state)
  (let ((server (daemon-state-server-socket state)))
    (loop until (state-stopped-p state)
          do (handler-case
                 (let ((client (usocket:socket-accept
                                server
                                :element-type '(unsigned-byte 8))))
                   (when client
                     (start-control-client-thread state client)))
               (error (condition)
                 (unless (state-stopped-p state)
                   (format *error-output*
                           "~&xmpp-cli daemon: control accept failed: ~a~%"
                           condition)
                   (finish-output *error-output*)
                   (error condition)))))))

(defun running-daemon-p ()
  (multiple-value-bind (ok response error) (daemon-status)
    (declare (ignore response error))
    ok))

(defun lock-owner-pid (lock)
  (getf lock :pid))

(defun stale-daemon-lock-p (lock)
  (let ((pid (lock-owner-pid lock)))
    (and pid
         (not (process-exists-p pid)))))

(defun fail-daemon-lock-held (lock)
  (let ((pid (and lock (lock-owner-pid lock))))
    (if pid
        (error "xmpp-cli agent daemon lock is held at ~a by pid ~d. If no daemon is running, remove that file."
               (namestring (daemon-lock-pathname))
               pid)
        (error "xmpp-cli agent daemon lock is held at ~a. If no daemon is running, remove that file."
               (namestring (daemon-lock-pathname))))))

(defun remove-stale-daemon-lock (lock)
  (let ((token (getf lock :token)))
    (delete-control token)
    (delete-stale-daemon-lock token)))

(defun acquire-daemon-lock-or-error (token)
  (let ((pid (current-process-id)))
    (unless (acquire-daemon-lock token :pid pid)
      (let ((lock (handler-case
                      (load-daemon-lock)
                    (error ()
                      nil))))
        (cond
          ((and lock (stale-daemon-lock-p lock))
           (format *error-output*
                   "~&xmpp-cli agent daemon: removing stale lock at ~a for dead pid ~d.~%"
                   (namestring (daemon-lock-pathname))
                   (lock-owner-pid lock))
           (finish-output *error-output*)
           (remove-stale-daemon-lock lock)
           (unless (acquire-daemon-lock token :pid pid)
             (fail-daemon-lock-held (ignore-errors (load-daemon-lock)))))
          (t
           (fail-daemon-lock-held lock))))))
  t)

(defun make-control (server profile-name profile-plist token)
  (list :pid (current-process-id)
        :host "127.0.0.1"
        :port (usocket:get-local-port server)
        :token token
        :started-at (now-iso8601)
        :profile profile-name
        :profile-jid (getf profile-plist :jid)
        :profile-digest (profile-digest profile-plist)))
