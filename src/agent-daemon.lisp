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
  (lock (bt:make-lock "xmpp-cli agent daemon")))

(defparameter *reply-whitespace* '(#\Space #\Tab #\Newline #\Return))

(defun current-process-id ()
  #+sbcl
  (or (ignore-errors (sb-posix:getpid)) nil)
  #-sbcl
  (let* ((package (find-package "SYSTEM"))
         (symbol (and package (find-symbol "GETPID" package))))
    (and symbol
         (fboundp symbol)
         (ignore-errors (funcall symbol)))))

(defun bare-jid (jid)
  (let ((text (or jid "")))
    (let ((slash (position #\/ text)))
      (if slash
          (subseq text 0 slash)
          text))))

(defun parse-agent-reply (body)
  (let ((trimmed (string-trim *reply-whitespace* (or body ""))))
    (when (plusp (length trimmed))
      (let ((end (position-if (lambda (char)
                                (member char *reply-whitespace* :test #'char=))
                              trimmed)))
        (if end
            (values (string-downcase (subseq trimmed 0 end))
                    (string-trim *reply-whitespace*
                                 (subseq trimmed end)))
            (values (string-downcase trimmed) ""))))))

(defun state-stopped-p (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (daemon-state-stop-p state)))

(defun request-stop (state)
  (let ((connection nil))
    (bt:with-lock-held ((daemon-state-lock state))
      (setf (daemon-state-stop-p state) t)
      (setf connection (daemon-state-connection state)))
    (when connection
      (ignore-errors
        (close-connection (daemon-state-backend state) connection)))))

(defun mark-connecting (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (daemon-state-xmpp-status state) :connecting)
    (setf (daemon-state-connection state) nil)
    (setf (daemon-state-connected-at state) nil)))

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
    (setf (daemon-state-last-error state) error-text)))

(defun route-count ()
  (handler-case
      (length (load-routes))
    (error ()
      nil)))

(defun status-plist (state)
  (let ((routes (route-count)))
    (bt:with-lock-held ((daemon-state-lock state))
      (let ((control (daemon-state-control state)))
        (list :ok t
              :pid (current-process-id)
              :profile (daemon-state-profile-name state)
              :control-host (getf control :host)
              :control-port (getf control :port)
              :xmpp-status (daemon-state-xmpp-status state)
              :connected (eq (daemon-state-xmpp-status state) :connected)
              :connected-at (daemon-state-connected-at state)
              :last-error (daemon-state-last-error state)
              :route-count routes)))))

(defun daemon-send-text (state to body)
  (let ((backend (daemon-state-backend state))
        (connection nil))
    (bt:with-lock-held ((daemon-state-lock state))
      (unless (and (eq (daemon-state-xmpp-status state) :connected)
                   (daemon-state-connection state))
        (return-from daemon-send-text
          (values nil "XMPP daemon connection is not ready.")))
      (setf connection (daemon-state-connection state))
      (handler-case
          (progn
            (send-connected-text backend connection to body)
            (values t nil))
        (error (condition)
          (let ((text (princ-to-string condition)))
            (setf (daemon-state-xmpp-status state) :disconnected)
            (setf (daemon-state-last-error state) text)
            (setf (daemon-state-connection state) nil)
            (values nil text)))))))

(defun send-daemon-note (state to text)
  (when (and to (plusp (length to)))
    (multiple-value-bind (ok error-text) (daemon-send-text state to text)
      (declare (ignore ok error-text)))))

(defun authorized-sender-p (state from)
  (let* ((sender (bare-jid from))
         (allowed (allowed-senders (daemon-state-agent-config state))))
    (and (plusp (length sender))
         (member sender allowed :test #'string-equal))))

(defun apply-route-reply (route text)
  (if (plusp (length text))
      (paste-text-and-enter route text)
      (focus-pane route)))

(defun handle-route-reply (state from code text)
  (let ((route (find-route-by-code code)))
    (cond
      ((null route)
       (send-daemon-note state
                         from
                         (format nil "xmpp-cli: unknown route code ~a" code)))
      (t
       (handler-case
           (progn
             (apply-route-reply route text)
             (mark-route-used route)
             (send-daemon-note
              state
              from
              (if (plusp (length text))
                  (format nil "xmpp-cli: sent feedback to ~a" (getf route :code))
                  (format nil "xmpp-cli: focused ~a" (getf route :code)))))
         (error (condition)
           (send-daemon-note
            state
            from
            (format nil "xmpp-cli: route ~a failed: ~a" code condition))))))))

(defun handle-incoming-message (state message)
  (let ((from (getf message :from))
        (body (getf message :body)))
    (when (and body (plusp (length body)))
      (cond
        ((not (authorized-sender-p state from))
         (format *error-output*
                 "~&xmpp-cli daemon: ignored message from unauthorized sender ~a~%"
                 (or from "<unknown>")))
        (t
         (multiple-value-bind (code text) (parse-agent-reply body)
           (when code
             (handle-route-reply state from code text))))))))

(defun sleep-until-stop (state seconds)
  (loop repeat seconds
        until (state-stopped-p state)
        do (sleep 1)))

(defun backoff-at (backoff index)
  (nth (min index (1- (length backoff))) backoff))

(defun xmpp-loop (state)
  (let* ((agent-config (daemon-state-agent-config state))
         (resource (getf agent-config :daemon-resource "xmpp-agent-helper"))
         (backoff (or (getf agent-config :reconnect-backoff-seconds)
                      '(1 2 5 10 30 60 300)))
         (backoff-index 0))
    (loop until (state-stopped-p state)
          do (progn
               (mark-connecting state)
               (handler-case
                   (progn
                     (call-with-connection
                      (daemon-state-backend state)
                      (daemon-state-profile state)
                      (lambda (connection)
                        (mark-connected state connection)
                        (setf backoff-index 0)
                        (receive-connected-message-loop
                         (daemon-state-backend state)
                         connection
                         (lambda (message)
                           (handle-incoming-message state message))))
                      :resource resource
                      :send-presence t)
                     (unless (state-stopped-p state)
                       (mark-disconnected state "XMPP connection closed.")))
                 (error (condition)
                   (unless (state-stopped-p state)
                     (mark-disconnected state (princ-to-string condition)))))
               (unless (state-stopped-p state)
                 (let ((delay (backoff-at backoff backoff-index)))
                   (format *error-output*
                           "~&xmpp-cli daemon: reconnecting in ~d second~:p~@[ after: ~a~]~%"
                           delay
                           (daemon-state-last-error state))
                   (finish-output *error-output*)
                   (sleep-until-stop state delay)
                   (incf backoff-index)))))))

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
        (let ((to (getf request :to))
              (body (getf request :body)))
          (if (and (stringp to) (plusp (length to)) (stringp body))
              (multiple-value-bind (ok error-text) (daemon-send-text state to body)
                (if ok
                    (list :ok t)
                    (list :ok nil :error error-text)))
              (list :ok nil :error "Malformed send request."))))
       (:stop
        (request-stop state)
        (list :ok t))
       (otherwise
        (list :ok nil :error "Unknown daemon operation."))))))

(defun handle-client (state socket)
  (unwind-protect
       (let ((stream (make-ipc-stream socket)))
         (handler-case
             (let* ((request (read-ipc-message stream))
                    (response (handle-control-request state request)))
               (write-ipc-message stream response))
           (error (condition)
             (ignore-errors
               (write-ipc-message
                stream
                (list :ok nil
                      :error (format nil "daemon request failed: ~a" condition)))))))
    (ignore-errors
      (usocket:socket-close socket))))

(defun accept-control-loop (state)
  (let ((server (daemon-state-server-socket state)))
    (loop until (state-stopped-p state)
          do (handler-case
                 (let ((client (usocket:socket-accept
                                server
                                :element-type '(unsigned-byte 8))))
                   (handle-client state client))
               (error (condition)
                 (unless (state-stopped-p state)
                   (format *error-output*
                           "~&xmpp-cli daemon: control accept failed: ~a~%"
                           condition)))))))

(defun running-daemon-p ()
  (multiple-value-bind (ok response error) (daemon-status)
    (declare (ignore response error))
    ok))

(defun make-control (server profile-name token)
  (list :pid (current-process-id)
        :host "127.0.0.1"
        :port (usocket:get-local-port server)
        :token token
        :started-at (now-iso8601)
        :profile profile-name))

(defun run-daemon (backend)
  (when (running-daemon-p)
    (error "xmpp-cli agent daemon is already running."))
  (let* ((agent-config (load-agent-config))
         (profile-name (getf agent-config :profile "default"))
         (state-config (load-config))
         (profile-plist (profile state-config profile-name)))
    (unless profile-plist
      (error "No auth/profile data found for profile ~a. Run xmpp-cli login first."
             profile-name))
    (let* ((server (usocket:socket-listen "127.0.0.1"
                                          0
                                          :reuse-address t
                                          :element-type '(unsigned-byte 8)))
           (token (make-control-token))
           (control (make-control server profile-name token))
           (state (make-daemon-state :backend backend
                                     :profile-name profile-name
                                     :profile profile-plist
                                     :agent-config agent-config
                                     :control control
                                     :server-socket server
                                     :token token))
           (xmpp-thread nil))
      (unwind-protect
           (progn
             (save-control control)
             (setf xmpp-thread
                   (bt:make-thread (lambda () (xmpp-loop state))
                                   :name "xmpp-cli XMPP daemon"))
             (format t "xmpp-cli agent daemon listening on 127.0.0.1:~d using profile ~a~%"
                     (getf control :port)
                     profile-name)
             (finish-output)
             (accept-control-loop state)
             0)
        (request-stop state)
        (ignore-errors
          (usocket:socket-close server))
        (when (and xmpp-thread (bt:thread-alive-p xmpp-thread))
          (ignore-errors
            (bt:destroy-thread xmpp-thread)))
        (delete-control)))))
