(in-package #:xmpp-cli/cli)

(defconstant +exit-success+ 0)
(defconstant +exit-general+ 1)
(defconstant +exit-usage+ 2)
(defconstant +exit-missing-profile+ 3)
(defconstant +exit-xmpp+ 4)

(defvar *backend-factory* #'xmpp-cli/backend/cl-xmpp:make-backend)

(define-condition cli-error (simple-error)
  ((exit-code
    :initarg :exit-code
    :reader cli-error-exit-code)))

(defun fail (exit-code control &rest arguments)
  (error 'cli-error
         :exit-code exit-code
         :format-control control
         :format-arguments arguments))

(defun option (cmd key &optional default)
  (clingon:getopt cmd key default))

(defun keyword-from-mechanism (value)
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return #\:)
                           (or value "auto"))))
    (unless (plusp (length text))
      (fail +exit-usage+ "Mechanism must be non-empty."))
    (intern (string-upcase text) :keyword)))

(defun read-password-from-stdin ()
  (let ((line (read-line *standard-input* nil nil)))
    (unless line
      (fail +exit-usage+ "--password-stdin was supplied, but no password was read."))
    (when (zerop (length line))
      (fail +exit-usage+ "Password read from stdin must be non-empty."))
    line))

(defun parse-login-jid (jid)
  (handler-case
      (split-jid jid)
    (error (condition)
      (fail +exit-usage+ "~a" condition))))

(defun validate-recipient-jid (jid)
  (unless (and (stringp jid) (plusp (length jid)) (position #\@ jid))
    (fail +exit-usage+ "Recipient JID must be a non-empty XMPP account such as user@example.org."))
  jid)

(defun login-options ()
  (list
   (clingon:make-option :string
                        :long-name "profile"
                        :description "Profile name"
                        :initial-value *default-profile-name*
                        :key :profile)
   (clingon:make-option :string
                        :long-name "host"
                        :description "XMPP host"
                        :key :host)
   (clingon:make-option :integer
                        :long-name "port"
                        :description "XMPP client port"
                        :initial-value 5222
                        :key :port)
   (clingon:make-option :string
                        :long-name "resource"
                        :description "XMPP resource"
                        :initial-value "xmpp-cli"
                        :key :resource)
   (clingon:make-option :string
                        :long-name "mechanism"
                        :description "SASL mechanism name"
                        :initial-value "auto"
                        :key :mechanism)
   (clingon:make-option :flag
                        :long-name "password-stdin"
                        :description "Read one password line from standard input"
                        :key :password-stdin)))

(defun send-options ()
  (list
   (clingon:make-option :string
                        :long-name "profile"
                        :description "Profile name"
                        :key :profile)
   (clingon:make-option :filepath
                        :short-name #\f
                        :long-name "file"
                        :description "Read message body from a UTF-8 text file"
                        :key :file)))

(defun command-usage-error (cmd message)
  (clingon:print-usage cmd *error-output*)
  (fail +exit-usage+ "~a" message))

(defun handle-login (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (unless (= (length args) 1)
      (fail +exit-usage+ "Usage: xmpp-cli login <jid> --password-stdin [options]"))
    (unless (option cmd :password-stdin)
      (fail +exit-usage+ "login requires --password-stdin."))
    (let* ((jid (first args))
           (profile-name (option cmd :profile *default-profile-name*))
           (host (option cmd :host))
           (port (option cmd :port 5222))
           (resource (option cmd :resource "xmpp-cli"))
           (mechanism (keyword-from-mechanism (option cmd :mechanism "auto")))
           (password (read-password-from-stdin)))
      (multiple-value-bind (username domain) (parse-login-jid jid)
        (let ((profile-plist (list :jid jid
                                   :username username
                                   :domain domain
                                   :host (or host domain)
                                   :port port
                                   :resource resource
                                   :mechanism mechanism
                                   :password password)))
          (handler-case
              (check-login (funcall *backend-factory*) profile-plist)
            (error (condition)
              (fail +exit-xmpp+ "login failed: ~a" condition)))
          (save-config
           (set-profile (load-config)
                        profile-name
                        profile-plist
                        :make-default t))
          (format t "logged in as ~a using profile ~a~%" jid profile-name)
          +exit-success+)))))

(defun read-message-body (file message-args)
  (cond
    ((and file message-args)
     (fail +exit-usage+ "send accepts either message text or -f/--file, not both."))
    (file
     (read-file-as-string file))
    ((null message-args)
     (fail +exit-usage+ "send requires message text or -f/--file."))
    ((/= (length message-args) 1)
     (fail +exit-usage+ "Message text must be one shell argument; quote it if it contains spaces."))
    (t
     (first message-args))))

(defun append-send-history (profile-name recipient kind body result &key error)
  (append-history :profile profile-name
                  :to recipient
                  :kind kind
                  :bytes (utf-8-byte-length body)
                  :sha256 (sha256-hex body)
                  :result result
                  :error error))

(defun maybe-append-send-history (profile-name recipient kind body result &key error)
  (handler-case
      (append-send-history profile-name recipient kind body result :error error)
    (error (condition)
      (format *error-output*
              "warning: could not record send history: ~a~%"
              condition)
      nil)))

(defun handle-send (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (recipient (first args))
         (message-args (rest args))
         (file (option cmd :file)))
    (unless recipient
      (fail +exit-usage+ "Usage: xmpp-cli send <recipient-jid> \"message text\""))
    (validate-recipient-jid recipient)
    (let* ((body (read-message-body file message-args))
           (kind (if file :file-text :text))
           (config (load-config))
           (profile-name (or (option cmd :profile)
                             (default-profile-name config)))
           (profile-plist (profile config profile-name)))
      (unless profile-plist
        (fail +exit-missing-profile+
              "No auth/profile data found for profile ~a. Run xmpp-cli login first."
              profile-name))
      (let ((send-error
              (handler-case
                  (progn
                    (send-text (funcall *backend-factory*) profile-plist recipient body)
                    nil)
                (error (condition)
                  condition))))
        (if send-error
            (progn
              (ignore-errors
                (append-send-history profile-name
                                     recipient
                                     kind
                                     body
                                     :failed
                                     :error (princ-to-string send-error)))
              (format *error-output* "send failed: ~a~%" send-error)
              +exit-xmpp+)
            (progn
              (maybe-append-send-history profile-name recipient kind body :sent)
              (format t "sent to ~a using profile ~a~%" recipient profile-name)
              +exit-success+))))))

(defun handle-agent-config-show (cmd)
  (declare (ignore cmd))
  (format t "~a" (emit-yaml (agent-config-as-yaml (load-agent-config))))
  +exit-success+)

(defun handle-agent-config-set-notify-to (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (unless (= (length args) 1)
      (command-usage-error cmd "Usage: xmpp-cli agent config set-notify-to <jid>"))
    (let* ((jid (first args))
           (config (set-notify-to (load-agent-config) jid)))
      (save-agent-config config)
      (format t "set XMPP notification target to ~a~%" jid)
      +exit-success+)))

(defun handle-agent-config-allow-sender (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (unless (= (length args) 1)
      (command-usage-error cmd "Usage: xmpp-cli agent config allow-sender <jid>"))
    (let* ((jid (first args))
           (config (add-allowed-sender (load-agent-config) jid)))
      (save-agent-config config)
      (format t "allowed XMPP sender ~a~%" jid)
      +exit-success+)))

(defun handle-agent-config-remove-sender (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (unless (= (length args) 1)
      (command-usage-error cmd "Usage: xmpp-cli agent config remove-sender <jid>"))
    (let* ((jid (first args))
           (config (remove-allowed-sender (load-agent-config) jid)))
      (save-agent-config config)
      (format t "removed XMPP sender ~a~%" jid)
      +exit-success+)))

(defun login-command ()
  (clingon:make-command
   :name "login"
   :description "store a verified XMPP login profile"
   :usage "<jid> --password-stdin [options]"
   :options (login-options)
   :handler #'handle-login
   :examples '(("Store the default profile:"
                . "printf '%s\\n' 'secret' | xmpp-cli login user@example.org --password-stdin"))))

(defun send-command ()
  (clingon:make-command
   :name "send"
   :description "send a text message to an XMPP JID"
   :usage "<recipient-jid> \"message text\" | <recipient-jid> -f FILE"
   :options (send-options)
   :handler #'handle-send
   :examples '(("Send literal text:"
                . "xmpp-cli send friend@example.org \"hello\"")
               ("Send UTF-8 text file contents:"
                . "xmpp-cli send friend@example.org -f ./message.txt"))))

(defun agent-config-show-command ()
  (clingon:make-command
   :name "show"
   :description "show XMPP agent bridge configuration"
   :handler #'handle-agent-config-show))

(defun agent-config-set-notify-to-command ()
  (clingon:make-command
   :name "set-notify-to"
   :description "set the XMPP notification recipient"
   :usage "<jid>"
   :handler #'handle-agent-config-set-notify-to))

(defun agent-config-allow-sender-command ()
  (clingon:make-command
   :name "allow-sender"
   :description "allow a JID to send agent replies"
   :usage "<jid>"
   :handler #'handle-agent-config-allow-sender))

(defun agent-config-remove-sender-command ()
  (clingon:make-command
   :name "remove-sender"
   :description "remove a JID from the allowed reply sender list"
   :usage "<jid>"
   :handler #'handle-agent-config-remove-sender))

(defun agent-config-handler (cmd)
  (clingon:print-usage cmd t)
  +exit-usage+)

(defun agent-config-command ()
  (clingon:make-command
   :name "config"
   :description "manage XMPP agent bridge configuration"
   :handler #'agent-config-handler
   :sub-commands (list (agent-config-show-command)
                       (agent-config-set-notify-to-command)
                       (agent-config-allow-sender-command)
                       (agent-config-remove-sender-command))))

(defun agent-handler (cmd)
  (clingon:print-usage cmd t)
  +exit-usage+)

(defun agent-command ()
  (clingon:make-command
   :name "agent"
   :description "manage XMPP agent bridge features"
   :handler #'agent-handler
   :sub-commands (list (agent-config-command))))

(defun top-level-handler (cmd)
  (clingon:print-usage cmd t)
  +exit-usage+)

(defun top-level-command ()
  (clingon:make-command
   :name "xmpp-cli"
   :description "send XMPP chat messages from the command line"
   :version "0.1.0"
   :license "MIT"
   :handler #'top-level-handler
   :sub-commands (list (login-command) (send-command) (agent-command))))

(defun usage-condition-p (condition)
  (or (typep condition 'clingon:unknown-option)
      (typep condition 'clingon:missing-option-argument)
      (typep condition 'clingon:missing-required-option-value)
      (typep condition 'clingon:option-derive-error)))

(defun run (argv)
  "Run xmpp-cli with ARGV excluding executable name. Return an exit code."
  (handler-case
      (let* ((app (top-level-command))
             (cmd (clingon:parse-command-line app argv))
             (handler (clingon:command-handler cmd)))
        (unless handler
          (clingon:print-usage cmd t)
          (return-from run +exit-usage+))
        (funcall handler cmd))
    (clingon:exit-error (condition)
      (let ((code (clingon:exit-error-code condition)))
        (if (zerop code) +exit-success+ +exit-usage+)))
    (cli-error (condition)
      (format *error-output* "~a~%" condition)
      (cli-error-exit-code condition))
    (error (condition)
      (cond
        ((usage-condition-p condition)
         (format *error-output* "~a~%" condition)
         +exit-usage+)
        (t
         (format *error-output* "xmpp-cli: ~a~%" condition)
         +exit-general+)))))
