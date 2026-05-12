(in-package #:xmpp-cli/agent-codex)

(defparameter *notification-detail-limit* 1800)

(defstruct notification
  target
  body
  route)

(defun read-codex-payload (&optional (stream *standard-input*))
  (let ((raw (read-stream-as-string stream)))
    (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
        (parse-json raw)
        nil)))

(defun string-value (value &optional default)
  (cond
    ((json-null-p value) default)
    ((null value) default)
    ((stringp value) value)
    (t (princ-to-string value))))

(defun payload-value (payload key &optional default)
  (string-value (json-value payload key nil) default))

(defun first-payload-value (payload keys &optional default)
  (dolist (key keys default)
    (let ((value (json-value payload key nil)))
      (unless (or (null value) (json-null-p value))
        (return (string-value value default))))))

(defun trim-detail (text &optional (limit *notification-detail-limit*))
  (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (or text ""))))
    (if (<= (length value) limit)
        value
        (concatenate 'string
                     (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                        (subseq value 0 (max 0 (1- limit))))
                     "..."))))

(defun host-name-fallback ()
  (let ((host (ignore-errors (machine-instance))))
    (if (and (stringp host) (plusp (length host)))
        host
        "unknown")))

(defun short-hostname (&optional host)
  (let* ((value (if (and (stringp host) (plusp (length host)))
                    host
                    (host-name-fallback)))
         (position (position #\. value)))
    (if position
        (subseq value 0 position)
        value)))

(defun current-directory-string ()
  (namestring (uiop:getcwd)))

(defun existing-directory-p (path)
  (and path
       (ignore-errors
         (uiop:directory-exists-p path))))

(defun git-root (cwd)
  (when (existing-directory-p cwd)
    (handler-case
        (let ((output (uiop:run-program (list "git"
                                              "-C"
                                              cwd
                                              "rev-parse"
                                              "--show-toplevel")
                                        :output :string
                                        :error-output nil
                                        :ignore-error-status t)))
          (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      output)))
            (and (plusp (length trimmed)) trimmed)))
      (error ()
        nil))))

(defun repository-location (cwd)
  (or (git-root cwd) cwd))

(defun event-title (event)
  (cond
    ((string= event "Stop") "codex finished")
    ((string= event "PermissionRequest") "codex needs input")
    (t (format nil "codex hook ~a" event))))

(defun summarize-tool-input (tool-input)
  (cond
    ((and (listp tool-input)
          (every (lambda (entry)
                   (and (consp entry) (stringp (car entry))))
                 tool-input))
     (let ((description (payload-value tool-input "description" nil))
           (command (payload-value tool-input "command" nil)))
       (cond
         ((and description command)
          (trim-detail (format nil "~a~%~a" description command)))
         (command
          (trim-detail command))
         (description
          (trim-detail description))
         (t
          (trim-detail (json-compact-string tool-input))))))
    ((or (null tool-input) (json-null-p tool-input))
     "")
    (t
     (trim-detail (json-compact-string tool-input)))))

(defun notification-common-lines (payload)
  (list (format nil "model: ~a" (payload-value payload "model" "unknown"))
        (format nil "turn: ~a" (payload-value payload "turn_id" "none"))))

(defun append-non-empty-detail (lines detail)
  (if (plusp (length detail))
      (append lines (list "" detail))
      lines))

(defun route-for-context (context payload config host cwd display-cwd)
  (when (context-available-p context)
    (let* ((agent-session (first-payload-value
                           payload
                           '("session_id"
                             "codex_session_id"
                             "conversation_id")
                           ""))
           (identity (canonical-route-identity
                      :host host
                      :tmux-socket (getf context :tmux-socket)
                      :tmux-session-id (getf context :tmux-session-id)
                      :tmux-window-id (getf context :tmux-window-id)
                      :tmux-pane-id (getf context :tmux-pane-id)
                      :agent :codex
                      :agent-session agent-session)))
      (handler-case
          (ensure-route identity
                        :code-length (getf config :code-length 4)
                        :agent :codex
                        :agent-session agent-session
                        :host host
                        :cwd cwd
                        :display-cwd display-cwd
                        :tmux-socket (getf context :tmux-socket)
                        :tmux-client-name (getf context :tmux-client-name)
                        :tmux-client-tty (getf context :tmux-client-tty)
                        :tmux-session-id (getf context :tmux-session-id)
                        :tmux-window-id (getf context :tmux-window-id)
                        :tmux-pane-id (getf context :tmux-pane-id))
        (error ()
          nil)))))

(defun build-header (route host display-repo event)
  (format nil "~a ~a ~a ~a"
          (or (getf route :code) "no-route")
          host
          display-repo
          (event-title event)))

(defun build-body (payload route host display-repo display-cwd)
  (let* ((event (payload-value payload "hook_event_name" "Codex"))
         (lines (list* (build-header route host display-repo event)
                       (notification-common-lines payload))))
    (setf lines (append lines (list (format nil "cwd: ~a" display-cwd))))
    (unless route
      (setf lines (append lines
                          (list "route: unavailable (not running inside tmux)"))))
    (cond
      ((string= event "Stop")
       (append-non-empty-detail
        lines
        (trim-detail (payload-value payload "last_assistant_message" ""))))
      ((string= event "PermissionRequest")
       (let ((permission-mode (payload-value payload "permission_mode" "unknown"))
             (detail (summarize-tool-input (json-value payload "tool_input"))))
         (append-non-empty-detail
          (append lines (list (format nil "permission: ~a" permission-mode)))
          detail)))
      (t
       lines))))

(defun build-codex-notification (payload config &key tmux-context host cwd)
  (let* ((target (notify-to config))
         (host (short-hostname host))
         (cwd (or (payload-value payload "cwd" nil)
                  cwd
                  (current-directory-string)))
         (repo (repository-location cwd))
         (display-repo (display-path repo))
         (display-cwd (display-path cwd))
         (context (or tmux-context (capture-context)))
         (route (route-for-context context payload config host cwd display-cwd))
         (body-lines (build-body payload route host display-repo display-cwd)))
    (make-notification :target target
                       :body (format nil "~{~a~^~%~}" body-lines)
                       :route route)))
