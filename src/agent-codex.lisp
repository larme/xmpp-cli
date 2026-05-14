(in-package #:xmpp-cli/agent-codex)

(defparameter *notification-message-limit* 1800)

(defstruct notification
  target
  body
  bodies
  route
  message-prefix-lines
  message-detail)

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

(defun codex-permission-request-p (payload)
  (string= (payload-value payload "hook_event_name" "")
           "PermissionRequest"))

(defun first-payload-value (payload keys &optional default)
  (dolist (key keys default)
    (let ((value (json-value payload key nil)))
      (unless (or (null value) (json-null-p value))
        (return (string-value value default))))))

(defun normalize-detail (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (or text "")))

(defun join-lines (lines)
  (with-output-to-string (out nil :element-type 'character)
    (loop for line in lines
          for first = t then nil
          do (progn
               (unless first
                 (terpri out))
               (write-string line out)))))

(defun split-boundary (text start limit)
  (let* ((end (min (length text) (+ start limit)))
         (newline (position #\Newline text
                            :start start
                            :end end
                            :from-end t))
         (space (position #\Space text
                          :start start
                          :end end
                          :from-end t))
         (candidate (or newline space))
         (minimum-boundary (+ start (floor limit 2))))
    (if (and candidate (>= candidate minimum-boundary))
        (1+ candidate)
        end)))

(defun split-text (text limit)
  (let ((chunks nil)
        (start 0)
        (length (length text)))
    (loop while (< start length)
          for end = (if (<= (- length start) limit)
                        length
                        (split-boundary text start limit))
          do (progn
               (push (subseq text start end) chunks)
               (setf start end)))
    (nreverse chunks)))

(defun part-marker (index total)
  (format nil "[~d/~d]" index total))

(defun body-lines-with-detail (prefix-lines detail)
  (append-non-empty-detail prefix-lines detail))

(defun part-overhead (prefix-lines total)
  (length (join-lines (append (list (part-marker total total))
                              prefix-lines
                              ;; One blank line separates metadata from the
                              ;; chunk; the second placeholder accounts for
                              ;; the newline before the chunk itself.
                              (list "" "")))))

(defun part-body (prefix-lines chunk index total)
  (join-lines (append (list (part-marker index total))
                      (body-lines-with-detail prefix-lines chunk))))

(defun split-notification-body (prefix-lines detail
                                &optional (limit *notification-message-limit*))
  (let ((body (join-lines (body-lines-with-detail prefix-lines detail))))
    (if (or (<= (length body) limit)
            (zerop (length detail)))
        (list body)
        (loop with total = 1
              for overhead = (part-overhead prefix-lines total)
              for chunk-limit = (max 1 (- limit overhead))
              for chunks = (split-text detail chunk-limit)
              for next-total = (length chunks)
              when (= next-total total)
                return (loop for chunk in chunks
                             for index from 1
                             collect (part-body prefix-lines
                                                chunk
                                                index
                                                total))
              do (setf total next-total)))))

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
          (normalize-detail (format nil "~a~%~a" description command)))
         (command
          (normalize-detail command))
         (description
          (normalize-detail description))
         (t
          (normalize-detail (json-compact-string tool-input))))))
    ((or (null tool-input) (json-null-p tool-input))
     "")
    (t
     (normalize-detail (json-compact-string tool-input)))))

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
                        :route-ttl-days (getf config :route-ttl-days)
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

(defun permission-tool-name (payload event)
  (when (string= event "PermissionRequest")
    (normalize-detail (payload-value payload "tool_name" ""))))

(defun permission-reply-lines (route)
  (let ((code (and route (getf route :code))))
    (append
     (list "reply options:")
     (if (and code (plusp (length code)))
         (list (format nil "allow: /choose ~a 1 (direct) or /choose 1 (room)"
                       code)
               (format nil "deny: /choose ~a 2 (direct) or /choose 2 (room)"
                       code))
         (list "allow: /choose 1"
               "deny: /choose 2")))))

(defun build-header (route host display-repo event &optional tool-name)
  (let ((header (format nil "~a ~a ~a ~a"
                        (or (getf route :code) "no-route")
                        host
                        display-repo
                        (event-title event))))
    (if (and tool-name (plusp (length tool-name)))
        (format nil "~a tool=~a" header tool-name)
        header)))

(defun notification-prefix-lines (payload
                                  route
                                  host
                                  display-repo
                                  display-cwd
                                  &key permission-replies-p)
  (let* ((event (payload-value payload "hook_event_name" "Codex"))
         (tool-name (permission-tool-name payload event))
         (lines (list* (build-header route
                                      host
                                      display-repo
                                      event
                                      tool-name)
                       (notification-common-lines payload))))
    (setf lines (append lines (list (format nil "cwd: ~a" display-cwd))))
    (unless route
      (setf lines (append lines
                          (list "route: unavailable (not running inside tmux)"))))
    (cond
      ((string= event "PermissionRequest")
       (append lines
               (list (format nil "permission: ~a"
                             (payload-value payload
                                            "permission_mode"
                                            "unknown")))
               (when permission-replies-p
                 (permission-reply-lines route))))
      (t
       lines))))

(defun notification-detail (payload)
  (let ((event (payload-value payload "hook_event_name" "Codex")))
    (cond
      ((string= event "Stop")
       (normalize-detail (payload-value payload "last_assistant_message" "")))
      ((string= event "PermissionRequest")
       (summarize-tool-input (json-value payload "tool_input")))
      (t
       ""))))

(defun build-body (prefix-lines detail)
  (join-lines (body-lines-with-detail prefix-lines detail)))

(defun notification-target-for-route (route fallback-jid)
  (if route
      (list :kind :route
            :route-id (getf route :route-id)
            :route-code (getf route :code)
            :fallback-jid fallback-jid)
      (list :kind :jid
            :jid fallback-jid)))

(defun make-notification-from-parts (target route prefix-lines detail)
  (make-notification :target target
                     :body (build-body prefix-lines detail)
                     :bodies (split-notification-body prefix-lines detail)
                     :route route
                     :message-prefix-lines prefix-lines
                     :message-detail detail))

(defun notification-with-permission-replies (notification)
  (let ((route (notification-route notification)))
    (unless route
      (error "Permission reply options require a route."))
    (make-notification-from-parts
     (notification-target notification)
     route
     (append (notification-message-prefix-lines notification)
             (permission-reply-lines route))
     (notification-message-detail notification))))

(defun build-codex-notification (payload config &key tmux-context host cwd)
  (let* ((fallback-jid (notify-to config))
         (host (short-hostname host))
         (cwd (or (payload-value payload "cwd" nil)
                  cwd
                  (current-directory-string)))
         (repo (repository-location cwd))
         (display-repo (display-path repo))
         (display-cwd (display-path cwd))
         (context (or tmux-context (capture-context)))
         (route (route-for-context context payload config host cwd display-cwd))
         (prefix-lines (notification-prefix-lines payload
                                                 route
                                                 host
                                                 display-repo
                                                 display-cwd))
         (detail (notification-detail payload))
         (target (notification-target-for-route route fallback-jid)))
    (make-notification-from-parts target route prefix-lines detail)))

(defun permission-decision-name (decision)
  (case decision
    (:allow "allow")
    (:deny "deny")
    (t (error "Unknown Codex permission decision: ~s" decision))))

(defun codex-permission-decision-json (decision &key message)
  (let ((decision-object
          (list (cons "behavior" (permission-decision-name decision)))))
    (when (and (eq decision :deny)
               message
               (plusp (length message)))
      (setf decision-object
            (append decision-object
                    (list (cons "message" message)))))
    (json-compact-string
     (list (cons "hookSpecificOutput"
                 (list (cons "hookEventName" "PermissionRequest")
                       (cons "decision" decision-object)))))))
