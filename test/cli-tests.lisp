(in-package #:xmpp-cli/test)

(defclass fake-backend () ())

(defvar *fake-events* nil)

(defmethod xmpp-cli/backend:check-login ((backend fake-backend) profile)
  (declare (ignore backend))
  (push (list :check-login profile) *fake-events*)
  t)

(defmethod xmpp-cli/backend:send-text ((backend fake-backend) profile to body)
  (declare (ignore backend))
  (push (list :send-text profile to body) *fake-events*)
  :sent)

(defun test-profile ()
  (list :jid "user@example.org"
        :username "user"
        :domain "example.org"
        :host "example.org"
        :port 5222
        :resource "xmpp-cli"
        :mechanism :sasl-plain
        :password "secret"))

(defun save-default-test-profile ()
  (xmpp-cli/state:save-config
   (xmpp-cli/state:set-profile (xmpp-cli/state:load-config)
                               "default"
                               (test-profile)
                               :make-default t)))

(defun run-cli (argv &key (input ""))
  (let ((*fake-events* nil)
        (xmpp-cli/cli::*backend-factory* (lambda () (make-instance 'fake-backend))))
    (with-input-from-string (*standard-input* input)
      (let ((*standard-output* (make-string-output-stream))
            (*error-output* (make-string-output-stream)))
        (values (xmpp-cli/cli:run argv)
                (reverse *fake-events*)
                (get-output-stream-string *standard-output*)
                (get-output-stream-string *error-output*))))))

(deftest cli-login-requires-password-stdin
  (with-isolated-data
    (multiple-value-bind (code events)
        (run-cli '("login" "user@example.org"))
      (check-equal 2 code)
      (check-equal nil events))))

(deftest cli-login-saves-after-check
  (with-isolated-data
    (multiple-value-bind (code events)
        (run-cli '("login" "user@example.org" "--password-stdin")
                 :input (format nil "secret~%"))
      (check-equal 0 code)
      (check-equal :check-login (caar events))
      (let* ((config (xmpp-cli/state:load-config))
             (profile (xmpp-cli/state:profile config "default")))
        (check-equal "user@example.org" (getf profile :jid))
        (check-equal "example.org" (getf profile :host))
        (check-equal :auto (getf profile :mechanism))
        (check-equal "secret" (getf profile :password))))))

(deftest cli-send-validation
  (with-isolated-data
    (multiple-value-bind (code events)
        (run-cli '("send" "friend@example.org"))
      (check-equal 2 code)
      (check-equal nil events))
    (let ((file (merge-pathnames "message.txt" xmpp-cli/util::*data-directory*)))
      (xmpp-cli/util:write-private-file file "hello")
      (multiple-value-bind (code events)
          (run-cli (list "send" "friend@example.org" "hello" "-f" (namestring file)))
        (check-equal 2 code)
        (check-equal nil events)))))

(deftest cli-send-records-history
  (with-isolated-data
    (save-default-test-profile)
    (multiple-value-bind (code events)
        (run-cli '("send" "friend@example.org" "hello"))
      (check-equal 0 code)
      (check-equal :send-text (caar events))
      (let* ((text (xmpp-cli/util:read-file-as-string
                    (xmpp-cli/history:history-pathname)))
             (history (xmpp-cli/history::load-history))
             (entry (first history)))
        (check-equal :sent (getf entry :result))
        (check-equal 5 (getf entry :bytes))
        (check (not (search "hello" text))
               "history should store hash and metadata, not message text")))))

(deftest cli-send-history-failure-does-not-fail-send
  (with-isolated-data
    (save-default-test-profile)
    (xmpp-cli/util:write-private-file
     (xmpp-cli/history:history-pathname)
     "42")
    (multiple-value-bind (code events output error-output)
        (run-cli '("send" "friend@example.org" "hello"))
      (declare (ignore output))
      (check-equal 0 code)
      (check-equal 1 (length events))
      (check-equal :send-text (caar events))
      (check (search "warning: could not record send history" error-output)
             "history persistence failure should emit a warning."))))
