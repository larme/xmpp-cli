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
(defparameter *daemon-thread-stop-wait-seconds* 5)
(defparameter *daemon-client-timeout-seconds* 2)

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
      (setf server (daemon-state-server-socket state)))
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

(defun route-ttl-days (state)
  (getf (daemon-state-agent-config state) :route-ttl-days))

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

(defun reply-text (body)
  (string-trim *reply-whitespace* (or body "")))

(defun command-text-p (text)
  (and (plusp (length text))
       (char= (char text 0) #\/)))

(defun route-code-character-p (char)
  (or (and (char>= char #\a)
           (char<= char #\z))
      (and (char>= char #\A)
           (char<= char #\Z))))

(defun route-code-token-p (state token)
  (let ((code-length (getf (daemon-state-agent-config state)
                           :code-length
                           4)))
    (and (stringp token)
         (= (length token) code-length)
         (every #'route-code-character-p token))))

(defun active-routes-for-state (state)
  (load-active-routes :route-ttl-days (route-ttl-days state)))

(defun resolve-route-reply (state body)
  (let ((message (reply-text body)))
    (when (plusp (length message))
      (let* ((routes (active-routes-for-state state))
             (ttl-days (route-ttl-days state)))
        (multiple-value-bind (candidate-code candidate-text)
            (parse-agent-reply message)
          (let ((explicit-route (and candidate-code
                                     (find-route-by-code candidate-code
                                                         routes
                                                         ttl-days))))
            (if explicit-route
                (values explicit-route candidate-text nil)
                (if (route-code-token-p state candidate-code)
                    (values nil nil nil candidate-code)
                    (let ((default-route (last-active-route routes)))
                      (when default-route
                        (values default-route message t)))))))))))

(defun route-reply-success-message (route text default-route-p)
  (let ((suffix (if default-route-p " (default route)" "")))
    (if (plusp (length text))
        (format nil "xmpp-cli: sent feedback to ~a~a" (getf route :code) suffix)
        (format nil "xmpp-cli: focused ~a~a" (getf route :code) suffix))))

(defun handle-resolved-route-reply (state from route text default-route-p)
  (handler-case
      (progn
        (apply-route-reply route text)
        (mark-route-used route)
        (send-daemon-note
         state
         from
         (route-reply-success-message route text default-route-p)))
    (error (condition)
      (send-daemon-note
       state
       from
       (format nil "xmpp-cli: route ~a failed: ~a"
               (getf route :code)
               condition)))))

(defun handle-route-reply (state from body)
  (multiple-value-bind (route text default-route-p unknown-route-code)
      (resolve-route-reply state body)
    (cond
      (route
       (handle-resolved-route-reply state from route text default-route-p))
      (unknown-route-code
       (send-daemon-note
        state
        from
        (format nil "xmpp-cli: route code ~a is unknown or stale; wait for a new Codex notification or use a current route code."
                unknown-route-code)))
      (t
       (send-daemon-note
        state
        from
        "xmpp-cli: no active route; wait for a Codex notification or prepend a route code.")))))

(defun parse-agent-command (body)
  (let ((text (reply-text body)))
    (when (command-text-p text)
      (let ((without-slash (subseq text 1)))
        (multiple-value-bind (name rest)
            (parse-agent-reply without-slash)
          (values name rest))))))

(defun split-command-arguments (text)
  (let ((trimmed (reply-text text)))
    (when (plusp (length trimmed))
      (loop with parts = nil
            with start = 0
            with length = (length trimmed)
            while (< start length)
            for end = (position-if (lambda (char)
                                     (member char
                                             *reply-whitespace*
                                             :test #'char=))
                                   trimmed
                                   :start start)
            do (progn
                 (push (subseq trimmed start end) parts)
                 (setf start
                       (or (and end
                                (position-if-not
                                 (lambda (char)
                                   (member char
                                           *reply-whitespace*
                                           :test #'char=))
                                 trimmed
                                 :start end))
                           length)))
            finally (return (nreverse parts))))))

(defun resolve-command-route (state route-code)
  (let* ((routes (active-routes-for-state state))
         (ttl-days (route-ttl-days state)))
    (if (and route-code (plusp (length route-code)))
        (find-route-by-code route-code routes ttl-days)
        (last-active-route routes))))

(defun plist-string (plist key)
  (let ((value (getf plist key)))
    (and (stringp value)
         (plusp (length value))
         value)))

(defun ensure-new-codex-route (state source-route new-context)
  (let* ((agent-session "")
         (host (plist-string source-route :host))
         (cwd (or (plist-string source-route :cwd)
                  (plist-string new-context :tmux-pane-current-path)))
         (display-cwd (if (and (string= (or cwd "")
                                        (or (getf source-route :cwd) ""))
                               (plist-string source-route :display-cwd))
                          (getf source-route :display-cwd)
                          (display-path cwd)))
         (socket (or (plist-string new-context :tmux-socket)
                     (plist-string source-route :tmux-socket)))
         (session-id (plist-string new-context :tmux-session-id))
         (window-id (plist-string new-context :tmux-window-id))
         (pane-id (plist-string new-context :tmux-pane-id))
         (identity (canonical-route-identity
                    :host host
                    :tmux-socket socket
                    :tmux-session-id session-id
                    :tmux-window-id window-id
                    :tmux-pane-id pane-id
                    :agent :codex
                    :agent-session agent-session)))
    (unless pane-id
      (error "New Codex pane context does not include a tmux pane id."))
    (unless cwd
      (error "New Codex pane route does not include a working directory."))
    (ensure-route identity
                  :code-length (getf (daemon-state-agent-config state)
                                     :code-length
                                     4)
                  :route-ttl-days (route-ttl-days state)
                  :agent :codex
                  :agent-session agent-session
                  :host host
                  :cwd cwd
                  :display-cwd display-cwd
                  :tmux-socket socket
                  :tmux-client-name (plist-string source-route
                                                  :tmux-client-name)
                  :tmux-client-tty (plist-string source-route
                                                 :tmux-client-tty)
                  :tmux-session-id session-id
                  :tmux-window-id window-id
                  :tmux-pane-id pane-id)))

(defun handle-new-command (state from arguments)
  (cond
    ((> (length arguments) 1)
     (send-daemon-note state from "xmpp-cli: usage: /new [route-code]"))
    (t
     (let* ((route-code (first arguments))
            (route (resolve-command-route state route-code)))
       (cond
         ((null route)
          (send-daemon-note
           state
           from
           (if route-code
               (format nil "xmpp-cli: unknown route code ~a" route-code)
               "xmpp-cli: no active route; wait for a Codex notification or pass /new <route-code>.")))
         (t
          (handler-case
              (let* ((new-context (start-codex-session route))
                     (updated-route (mark-route-used route))
                     (new-route (ensure-new-codex-route state
                                                        updated-route
                                                        new-context)))
                (send-daemon-note
                 state
                 from
                 (format nil "xmpp-cli: started new Codex session ~a from ~a in window ~a pane ~a"
                         (getf new-route :code)
                         (getf route :code)
                         (or (getf new-route :tmux-window-id) "unknown")
                         (getf new-route :tmux-pane-id))))
            (error (condition)
              (send-daemon-note
               state
               from
               (format nil "xmpp-cli: /new failed for ~a: ~a"
                       (getf route :code)
                       condition))))))))))

(defun handle-agent-command (state from body)
  (multiple-value-bind (name rest)
      (parse-agent-command body)
    (cond
      ((null name)
       nil)
      ((string= name "new")
       (handle-new-command state from (split-command-arguments rest)))
      (t
       (send-daemon-note
        state
        from
        (format nil "xmpp-cli: unknown command /~a" name))))))

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
         (let ((text (reply-text body)))
           (if (command-text-p text)
               (handle-agent-command state from text)
               (handle-route-reply state from text))))))))

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

(defun lock-control-pid (lock)
  (handler-case
      (let ((control (load-control)))
        (and control
             lock
             (string= (getf lock :token) (getf control :token))
             (getf control :pid)))
    (error ()
      nil)))

(defun lock-owner-pid (lock)
  (or (getf lock :pid)
      (lock-control-pid lock)))

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

(defun wait-for-thread-stop (thread seconds)
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop while (and (bt:thread-alive-p thread)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.1))
    (not (bt:thread-alive-p thread))))

(defun cleanup-daemon (state server xmpp-thread token lock-acquired)
  (when state
    (ignore-errors
      (request-stop state)))
  (when server
    (ignore-errors
      (usocket:socket-close server)))
  (when (and xmpp-thread (bt:thread-alive-p xmpp-thread))
    (unless (wait-for-thread-stop xmpp-thread *daemon-thread-stop-wait-seconds*)
      (format *error-output*
              "~&xmpp-cli daemon: XMPP thread did not stop cleanly; destroying it.~%")
      (finish-output *error-output*)
      (ignore-errors
        (bt:destroy-thread xmpp-thread))))
  (delete-control token)
  (when lock-acquired
    (release-daemon-lock token)))

(defun run-daemon (backend)
  (let* ((agent-config (load-agent-config))
         (profile-name (getf agent-config :profile "default"))
         (state-config (load-config))
         (profile-plist (profile state-config profile-name))
         (token (make-control-token))
         (lock-acquired nil)
         (server nil)
         (state nil)
         (xmpp-thread nil))
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
             (format t "xmpp-cli agent daemon listening on 127.0.0.1:~d using profile ~a~%"
                     (getf control :port)
                     profile-name)
             (finish-output)
             (accept-control-loop state)
             0))
      (cleanup-daemon state server xmpp-thread token lock-acquired))))
