(in-package #:xmpp-cli/tmux)

(defparameter *tmux-display-format*
  "#{session_id}\t#{session_name}\t#{window_id}\t#{window_index}\t#{window_name}\t#{pane_id}\t#{pane_index}\t#{pane_current_path}")

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

(defun parse-tmux-display-line (line tmux-env)
  (let ((parts (split-string (trim-line-end line) #\Tab)))
    (when (= (length parts) 8)
      (destructuring-bind (session-id
                           session-name
                           window-id
                           window-index
                           window-name
                           pane-id
                           pane-index
                           pane-current-path)
          parts
        (list :tmux-socket (first-tmux-field tmux-env)
              :tmux-session-id session-id
              :tmux-session-name session-name
              :tmux-window-id window-id
              :tmux-window-index window-index
              :tmux-window-name window-name
              :tmux-pane-id pane-id
              :tmux-pane-index pane-index
              :tmux-pane-current-path pane-current-path)))))

(defun fallback-tmux-context (tmux-env pane)
  (when (and tmux-env pane)
    (list :tmux-socket (first-tmux-field tmux-env)
          :tmux-pane-id pane)))

(defun run-tmux (arguments &key input socket)
  (uiop:run-program (append (list "tmux")
                            (when socket (list "-S" socket))
                            arguments)
                    :input (or input nil)
                    :output :string
                    :error-output nil
                    :ignore-error-status t))

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

(defun focus-pane (route)
  (let ((socket (getf route :tmux-socket))
        (window-id (getf route :tmux-window-id))
        (pane-id (getf route :tmux-pane-id)))
    (unless pane-id
      (error "Route does not include a tmux pane target."))
    (when window-id
      (run-tmux (list "select-window" "-t" window-id) :socket socket))
    (run-tmux (list "select-pane" "-t" pane-id) :socket socket)
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
  (focus-pane route)
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
           (run-tmux (list "paste-buffer" "-d" "-b" buffer-name "-t" pane-id)
                     :socket (getf route :tmux-socket))
           (run-tmux (list "send-keys" "-t" pane-id "Enter")
                     :socket (getf route :tmux-socket))
           t)
      (ignore-errors
        (delete-file temp)))))
