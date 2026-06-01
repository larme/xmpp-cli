(in-package #:xmpp-cli/tmux)

(defun tab-joined-format (&rest fields)
  (with-output-to-string (out)
    (loop for field in fields
          for first = t then nil
          do (progn
               (unless first
                 (write-char #\Tab out))
               (write-string field out)))))

(defparameter *tmux-display-format*
  (tab-joined-format "#{client_name}"
                     "#{client_tty}"
                     "#{session_id}"
                     "#{session_name}"
                     "#{window_id}"
                     "#{window_index}"
                     "#{window_name}"
                     "#{pane_id}"
                     "#{pane_index}"
                     "#{pane_current_path}"))

(defparameter *tmux-pane-location-format*
  (tab-joined-format "#{client_name}"
                     "#{client_tty}"
                     "#{session_id}"
                     "#{window_id}"))

(defparameter *tmux-client-format*
  (tab-joined-format "#{client_name}"
                     "#{session_id}"))

(defparameter *post-paste-enter-delay* 0.25)
(defparameter *codex-current-command-names*
  ;; The npm-distributed Codex CLI runs under node, so tmux reports
  ;; pane_current_command as "node" even though the pane is a live Codex CLI.
  '("codex" "codex.js" "node"))

(defun split-string (string delimiter)
  (let ((parts nil)
        (start 0))
    (loop for position = (position delimiter string :start start)
          do (push (subseq string start position) parts)
          if position
            do (setf start (1+ position))
          else
            return (nreverse parts))))

(defun first-tmux-field (tmux-env)
  (and tmux-env
       (first (split-string tmux-env #\,))))

(defun trim-line-end (string)
  (string-right-trim '(#\Newline #\Return #\Space #\Tab) string))

(defun non-empty-lines (string)
  (remove "" (split-string (trim-line-end string) #\Newline) :test #'string=))

(defvar *tmux-executable* nil)
(defvar *codex-executable* nil)

(defun slash-suffixed (directory)
  (if (and (plusp (length directory))
           (char= (char directory (1- (length directory))) #\/))
      directory
      (concatenate 'string directory "/")))

(defun executable-candidates (name)
  (let* ((path (or (uiop:getenv "PATH") ""))
         (path-directories (remove "" (split-string path #\:) :test #'string=)))
    (if (position #\/ name)
        (list name)
        (remove-duplicates
         (append (mapcar (lambda (directory)
                           (concatenate 'string (slash-suffixed directory) name))
                         path-directories)
                 (list (concatenate 'string "/usr/bin/" name)
                       (concatenate 'string "/bin/" name)
                       (concatenate 'string "/usr/local/bin/" name)))
         :test #'string=))))

(defun find-executable (name)
  (loop for candidate in (executable-candidates name)
        for truename = (probe-file candidate)
        when truename
          return (namestring truename)))

(defun tmux-executable ()
  (or *tmux-executable*
      (setf *tmux-executable*
            (or (find-executable "tmux")
                (error "tmux executable was not found in PATH or common locations.")))))

(defun codex-executable ()
  (or *codex-executable*
      (setf *codex-executable*
            (or (find-executable "codex")
                (error "codex executable was not found in PATH or common locations.")))))

(defun tmux-command (arguments socket)
  (append (list (tmux-executable))
          (when socket (list "-S" socket))
          arguments))

#+lispworks
(defun lispworks-temp-output-pathname ()
  (merge-pathnames
   (format nil "xmpp-cli-tmux-~36r-~36r.out"
           (get-universal-time)
           (random 1000000000))
   (uiop:temporary-directory)))

;; Do not use UIOP:RUN-PROGRAM for tmux in delivered LispWorks images.
;; We hit a delivery-only failure where UIOP's LispWorks subprocess path
;; referenced SYSTEM:PIPE-EXIT-STATUS after delivery had removed or hidden it.
;; Keeping the LispWorks-specific process boundary here also lets us control
;; UTF-8 character output and exact exit-status handling.
#+lispworks
(defun run-command-lispworks (command capture-output)
  (let ((output-file (and capture-output (lispworks-temp-output-pathname))))
    (unwind-protect
         (multiple-value-bind (exit-status signal-number)
             (funcall (find-symbol "RUN-SHELL-COMMAND" "SYSTEM")
                      command
                      :wait t
                      :output (or output-file nil)
                      :error-output nil
                      :if-output-exists :supersede
                      :element-type 'character
                      :external-format :utf-8)
           (cond
             ((and exit-status (zerop exit-status) (null signal-number))
              (if output-file
                  (read-file-as-string output-file)
                  ""))
             (signal-number
              (error "tmux command was terminated by signal ~d: ~{~a~^ ~}"
                     signal-number
                     command))
             (t
              (error "tmux command exited with status ~a: ~{~a~^ ~}"
                     exit-status
                     command))))
      (when output-file
        (ignore-errors
          (delete-file output-file))))))

(defun parse-tmux-display-line (line tmux-env)
  (let ((parts (split-string (trim-line-end line) #\Tab)))
    (when (= (length parts) 10)
      (destructuring-bind (client-name
                           client-tty
                           session-id
                           session-name
                           window-id
                           window-index
                           window-name
                           pane-id
                           pane-index
                           pane-current-path)
          parts
        (list :tmux-socket (first-tmux-field tmux-env)
              :tmux-client-name client-name
              :tmux-client-tty client-tty
              :tmux-session-id session-id
              :tmux-session-name session-name
              :tmux-window-id window-id
              :tmux-window-index window-index
              :tmux-window-name window-name
              :tmux-pane-id pane-id
              :tmux-pane-index pane-index
              :tmux-pane-current-path pane-current-path)))))

(defun parse-tmux-pane-location-line (line)
  (let ((parts (split-string (trim-line-end line) #\Tab)))
    (cond
      ((= (length parts) 4)
       (destructuring-bind (client-name client-tty session-id window-id) parts
         (list :tmux-client-name client-name
               :tmux-client-tty client-tty
               :tmux-session-id session-id
               :tmux-window-id window-id)))
      ((= (length parts) 2)
       (destructuring-bind (session-id window-id) parts
         (list :tmux-session-id session-id
               :tmux-window-id window-id))))))

(defun parse-tmux-client-line (line)
  (let ((parts (split-string (trim-line-end line) #\Tab)))
    (when (= (length parts) 2)
      (destructuring-bind (client-name session-id) parts
        (list :tmux-client-name client-name
              :tmux-session-id session-id)))))

(defun fallback-tmux-context (tmux-env pane)
  (when (and tmux-env pane)
    (list :tmux-socket (first-tmux-field tmux-env)
          :tmux-pane-id pane)))

(defun run-tmux (arguments &key socket)
  (let ((command (tmux-command arguments socket)))
    ;; Non-LispWorks implementations keep the portable UIOP path. LispWorks
    ;; delivery uses RUN-COMMAND-LISPWORKS for the reasons documented above.
    #+lispworks
    (run-command-lispworks command t)
    #-lispworks
    (uiop:run-program command
                      :output :string
                      :error-output nil)))

(defun capture-context ()
  (let ((tmux-env (uiop:getenv "TMUX"))
        (pane (uiop:getenv "TMUX_PANE")))
    (when (and tmux-env pane)
      (or (handler-case
              (let ((output (run-tmux (list "display-message"
                                            "-p"
                                            "-t"
                                            pane
                                            *tmux-display-format*)
                                      :socket (first-tmux-field tmux-env))))
                (parse-tmux-display-line output tmux-env))
            (error ()
              nil))
          (fallback-tmux-context tmux-env pane)))))

(defun context-available-p (context)
  (and context
       (getf context :tmux-socket)
       (getf context :tmux-pane-id)))

(defun pane-location (pane-id socket)
  (parse-tmux-pane-location-line
   (run-tmux (list "display-message"
                   "-p"
                   "-t"
                   pane-id
                   *tmux-pane-location-format*)
             :socket socket)))

(defun pane-exists-p (route)
  (let ((pane-id (getf route :tmux-pane-id))
        (socket (getf route :tmux-socket)))
    (and pane-id
         (handler-case
             (string= pane-id
                      (trim-line-end
                       (run-tmux (list "display-message"
                                       "-p"
                                       "-t"
                                       pane-id
                                       "#{pane_id}")
                                 :socket socket)))
           (error ()
             nil)))))

(defun window-exists-p (route)
  (let ((window-id (getf route :tmux-window-id))
        (socket (getf route :tmux-socket)))
    (and window-id
         (handler-case
             (string= window-id
                      (trim-line-end
                       (run-tmux (list "display-message"
                                       "-p"
                                       "-t"
                                       window-id
                                       "#{window_id}")
                                 :socket socket)))
           (error ()
             nil)))))

(defun pane-current-command (route)
  (let ((pane-id (getf route :tmux-pane-id))
        (socket (getf route :tmux-socket)))
    (when pane-id
      (handler-case
          (let ((command (trim-line-end
                          (run-tmux (list "display-message"
                                          "-p"
                                          "-t"
                                          pane-id
                                          "#{pane_current_command}")
                                    :socket socket))))
            (and (plusp (length command)) command))
        (error ()
          nil)))))

(defun codex-current-command-p (command)
  (and command
       (member command *codex-current-command-names*
               :test #'string-equal)))

(defun codex-pane-active-p (route)
  (and (pane-exists-p route)
       (codex-current-command-p (pane-current-command route))))

(defun list-clients (&key session-id socket)
  (handler-case
      (let ((arguments (append (list "list-clients")
                               (when session-id
                                 (list "-t" session-id))
                               (list "-F" *tmux-client-format*))))
        (loop for line in (non-empty-lines (run-tmux arguments :socket socket))
              for client = (parse-tmux-client-line line)
              when client
                collect client))
    (error ()
      nil)))

(defun client-names (&key session-id socket)
  (remove-duplicates
   (loop for client in (list-clients :session-id session-id :socket socket)
         for client-name = (getf client :tmux-client-name)
         when (and client-name (plusp (length client-name)))
           collect client-name)
   :test #'string=))

(defun target-client-names (route location session-id socket)
  (let ((stored-client (getf route :tmux-client-name))
        (location-client (getf location :tmux-client-name)))
    (cond
      ((and stored-client
            (member stored-client (client-names :socket socket) :test #'string=))
       (list stored-client))
      ((and location-client
            (plusp (length location-client)))
       (list location-client))
      (session-id
       (client-names :session-id session-id :socket socket))
      (t
       nil))))

(defun select-pane-location (pane-id window-id socket)
  (when window-id
    (run-tmux (list "select-window" "-t" window-id) :socket socket))
  (run-tmux (list "select-pane" "-t" pane-id) :socket socket))

(defun focus-client (client-name session-id pane-id window-id socket)
  (declare (ignore session-id window-id))
  (when (and client-name pane-id)
    (run-tmux (list "switch-client" "-c" client-name "-t" pane-id)
              :socket socket))
  t)

(defun focus-pane (route)
  (let ((socket (getf route :tmux-socket))
        (session-id (getf route :tmux-session-id))
        (window-id (getf route :tmux-window-id))
        (pane-id (getf route :tmux-pane-id))
        (location nil))
    (unless pane-id
      (error "Route does not include a tmux pane target."))
    (unless (and session-id window-id)
      (setf location (pane-location pane-id socket))
      (setf session-id (or session-id (getf location :tmux-session-id)))
      (setf window-id (getf location :tmux-window-id)))
    (select-pane-location pane-id window-id socket)
    (dolist (client-name (target-client-names route location session-id socket))
      (focus-client client-name session-id pane-id window-id socket))
    t))

(defun agent-tmp-directory ()
  (merge-pathnames "agent/tmp/" (home-xmpp-cli-directory)))

(defun ensure-agent-tmp-directory ()
  (ensure-private-directory (agent-tmp-directory)))

(defun paste-temp-pathname (code)
  (merge-pathnames
   (format nil "xmpp-agent-~a-~36r.txt"
           (or code "route")
           (random 1000000000))
   (agent-tmp-directory)))

(defun paste-text-and-enter (route text)
  (ensure-agent-tmp-directory)
  (let* ((code (getf route :code))
         (buffer-name (format nil "xmpp-agent-~a" (or code "route")))
         (pane-id (getf route :tmux-pane-id))
         (temp (paste-temp-pathname code)))
    (unwind-protect
         (progn
           (write-private-file temp text)
           (run-tmux (list "load-buffer" "-b" buffer-name (namestring temp))
                     :socket (getf route :tmux-socket))
           (run-tmux (list "paste-buffer" "-p" "-d" "-b" buffer-name "-t" pane-id)
                     :socket (getf route :tmux-socket))
           (sleep *post-paste-enter-delay*)
           (run-tmux (list "send-keys" "-t" pane-id "C-m")
                     :socket (getf route :tmux-socket))
           t)
      (ignore-errors
        (delete-file temp)))))

(defun send-escape-key (route)
  (let ((pane-id (getf route :tmux-pane-id)))
    (unless pane-id
      (error "Route does not include a tmux pane target."))
    (run-tmux (list "send-keys" "-t" pane-id "Escape")
              :socket (getf route :tmux-socket))
    t))

(defun route-session-id (route)
  (let ((session-id (getf route :tmux-session-id))
        (pane-id (getf route :tmux-pane-id))
        (socket (getf route :tmux-socket)))
    (or session-id
        (getf (pane-location pane-id socket) :tmux-session-id))))

(defun new-codex-context (pane-id socket cwd)
  (let ((location (pane-location pane-id socket)))
    (append (list :tmux-socket socket
                  :tmux-pane-id pane-id
                  :tmux-pane-current-path cwd)
            location)))

(defun codex-command-arguments (resume-session-id)
  (let ((codex (codex-executable)))
    (if (and resume-session-id
             (plusp (length resume-session-id)))
        (list codex "resume" resume-session-id)
        (list codex))))

(defun start-codex-in-window-command (window-id cwd resume-session-id)
  (append (list "split-window"
                "-d"
                "-P"
                "-F"
                "#{pane_id}"
                "-t"
                window-id
                "-c"
                cwd)
          (codex-command-arguments resume-session-id)))

(defun start-codex-in-session-command (session-id cwd resume-session-id
                                       &key detach-p)
  (append (list "new-window")
          (when detach-p (list "-d"))
          (list "-P"
                "-F"
                "#{pane_id}"
                "-t"
                session-id
                "-c"
                cwd)
          (codex-command-arguments resume-session-id)))

(defun start-codex-session (route &key resume-session-id
                                      (focus-source-p t)
                                      prefer-existing-window-p)
  (let ((pane-id (getf route :tmux-pane-id))
        (cwd (getf route :cwd))
        (socket (getf route :tmux-socket)))
    (unless pane-id
      (error "Route does not include a tmux pane target."))
    (unless (and cwd (plusp (length cwd)))
      (error "Route does not include a working directory."))
    (let ((session-id (route-session-id route)))
      (unless session-id
        (error "Route does not include a tmux session target."))
      (when focus-source-p
        (focus-pane route))
      (let* ((window-id (getf route :tmux-window-id))
             (command (if (and prefer-existing-window-p
                               (window-exists-p route))
                          (start-codex-in-window-command window-id
                                                         cwd
                                                         resume-session-id)
                          (start-codex-in-session-command session-id
                                                          cwd
                                                          resume-session-id
                                                          :detach-p
                                                          (not focus-source-p))))
             (new-pane-id (trim-line-end
                           (run-tmux command :socket socket))))
        (unless (plusp (length new-pane-id))
          (error "tmux Codex launch did not return a pane id."))
        (new-codex-context new-pane-id socket cwd)))))
