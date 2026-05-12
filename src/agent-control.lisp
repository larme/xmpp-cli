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
              (body (getf request :body))
              (expected-profile-digest
                (getf request :expected-profile-digest))
              (actual-profile-digest
                (getf (daemon-state-control state) :profile-digest)))
          (cond
            ((not (and (stringp expected-profile-digest)
                       (stringp actual-profile-digest)
                       (string= expected-profile-digest
                                actual-profile-digest)))
             (list :ok nil
                   :error "Daemon profile does not match send request."))
            ((and (stringp to) (plusp (length to)) (stringp body))
             (multiple-value-bind (ok error-text) (daemon-send-text state to body)
               (if ok
                   (list :ok t)
                   (list :ok nil :error error-text))))
            (t
             (list :ok nil :error "Malformed send request.")))))
       (:stop
        (request-stop state)
        (list :ok t))
       (otherwise
        (list :ok nil :error "Unknown daemon operation."))))))

(defun handle-client (state socket)
  (unwind-protect
       (let ((stream (make-ipc-stream socket)))
         (handler-case
             (let* ((request (read-ipc-message-with-timeout
                              socket
                              stream
                              *daemon-client-timeout-seconds*
                              "Daemon IPC request"))
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
                   (start-control-client-thread state client))
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
