(in-package #:xmpp-cli/agent-daemon)

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
                        (start-room-rejoin-worker state)
                        (receive-connected-message-loop
                         (daemon-state-backend state)
                         connection
                         (lambda (stanza)
                           (handle-incoming-stanza state stanza))))
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

(defun handle-unmatched-iq (state stanza)
  (declare (ignore state))
  (when (member (getf stanza :type) '("get" "set") :test #'string-equal)
    (format *error-output*
            "~&xmpp-cli daemon: ignored unsupported IQ ~@[id=~a ~]type=~a~%"
            (getf stanza :id)
            (getf stanza :type))
    (finish-output *error-output*)))

(defun handle-incoming-presence (state stanza)
  (remember-room-occupant state stanza)
  (resolve-pending-room-join state stanza)
  nil)

(defun handle-incoming-groupchat (state stanza)
  (handle-room-message state stanza))

(defun handle-incoming-stanza (state stanza)
  (case (getf stanza :kind)
    (:iq
     (unless (resolve-pending-iq state stanza)
       (handle-unmatched-iq state stanza)))
    (:presence
     (handle-incoming-presence state stanza))
    (:groupchat
     (handle-incoming-groupchat state stanza))
    (:message
     (handle-incoming-message state stanza))
    (t
     nil)))

(defun wait-for-thread-stop (thread seconds)
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop while (and (bt:thread-alive-p thread)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.1))
    (not (bt:thread-alive-p thread))))

(defun stop-worker-thread (thread label)
  (when (and thread (bt:thread-alive-p thread))
    (unless (wait-for-thread-stop thread *daemon-thread-stop-wait-seconds*)
      (format *error-output*
              "~&xmpp-cli daemon: ~a thread did not stop cleanly; destroying it.~%"
              label)
      (finish-output *error-output*)
      (ignore-errors
        (bt:destroy-thread thread)))))

(defun cleanup-daemon (state server xmpp-thread room-cleanup-thread token lock-acquired)
  (when state
    (ignore-errors
      (request-stop state)))
  (when server
    (ignore-errors
      (usocket:socket-close server)))
  (stop-worker-thread room-cleanup-thread "room cleanup")
  (stop-worker-thread xmpp-thread "XMPP")
  (delete-control token)
  (when lock-acquired
    (release-daemon-lock token)))

(defun room-cleanup-loop (state)
  (loop until (state-stopped-p state)
        do (progn
             (handler-case
                 (cleanup-stale-rooms state)
               (error (condition)
                 (format *error-output*
                         "~&xmpp-cli daemon: room cleanup failed: ~a~%"
                         condition)
                 (finish-output *error-output*)))
             (sleep-until-stop state 300))))

(defun run-daemon (backend)
  (let* ((agent-config (load-agent-config))
         (profile-name (getf agent-config :profile))
         (state-config (load-config))
         (profile-plist (profile state-config profile-name))
         (token (make-control-token))
         (lock-acquired nil)
         (server nil)
         (state nil)
         (xmpp-thread nil)
         (room-cleanup-thread nil))
    (unless profile-plist
      (error "No auth/profile data found for profile ~a. Run xmpp-cli login first."
             profile-name))
    (unwind-protect
         (progn
           (acquire-daemon-lock-or-error token)
           (setf lock-acquired t)
           (when (running-daemon-p)
             (error "xmpp-cli agent daemon is already running."))
           (setf server (usocket:socket-listen "127.0.0.1"
                                               0
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8)))
           (let ((control (make-control server
                                        profile-name
                                        profile-plist
                                        token)))
             (setf state (make-daemon-state :backend backend
                                            :profile-name profile-name
                                            :profile profile-plist
                                            :agent-config agent-config
                                            :control control
                                            :server-socket server
                                            :token token))
             (save-control control)
             (setf xmpp-thread
                   (bt:make-thread (lambda () (xmpp-loop state))
                                   :name "xmpp-cli XMPP daemon"))
             (setf room-cleanup-thread
                   (bt:make-thread (lambda () (room-cleanup-loop state))
                                   :name "xmpp-cli room cleanup"))
             (format t "xmpp-cli agent daemon listening on 127.0.0.1:~d using profile ~a~%"
                     (getf control :port)
                     profile-name)
             (finish-output)
             (accept-control-loop state)
             0))
      (cleanup-daemon state
                      server
                      xmpp-thread
                      room-cleanup-thread
                      token
                      lock-acquired))))
