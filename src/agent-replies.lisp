(in-package #:xmpp-cli/agent-daemon)

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
