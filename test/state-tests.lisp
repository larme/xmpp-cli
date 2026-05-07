(in-package #:xmpp-cli/test)

(defvar *reader-eval-ran* nil)

(defun reader-eval-payload (result-form)
  (format nil "#.(progn (setf xmpp-cli/test::*reader-eval-ran* t) ~a)"
          result-form))

(deftest split-jid-valid
  (multiple-value-bind (user domain)
      (xmpp-cli/util:split-jid "user@example.org")
    (check-equal "user" user)
    (check-equal "example.org" domain)))

(deftest split-jid-invalid
  (check-signals-error
    (xmpp-cli/util:split-jid "missing-at"))
  (check-signals-error
    (xmpp-cli/util:split-jid "@example.org"))
  (check-signals-error
    (xmpp-cli/util:split-jid "user@")))

(deftest config-round-trip
  (with-isolated-data
    (let* ((profile (list :jid "user@example.org"
                          :username "user"
                          :domain "example.org"
                          :host "example.org"
                          :port 5222
                          :resource "xmpp-cli"
                          :mechanism :sasl-plain
                          :password "secret"))
           (config (xmpp-cli/state:set-profile (xmpp-cli/state:load-config)
                                               "default"
                                               profile
                                               :make-default t)))
      (xmpp-cli/state:save-config config)
      (let ((loaded (xmpp-cli/state:load-config)))
        (check-equal "default" (xmpp-cli/state:default-profile-name loaded))
        (check-equal profile (xmpp-cli/state:profile loaded "default"))))))

(deftest config-reader-eval-disabled
  (with-isolated-data
    (setf *reader-eval-ran* nil)
    (xmpp-cli/util:write-private-file
     (xmpp-cli/state:config-pathname)
     (reader-eval-payload
      "(list :default-profile \"default\" :profiles nil)"))
    (check-signals-error
      (xmpp-cli/state:load-config))
    (check (not *reader-eval-ran*)
           "config.sexp reader eval should stay disabled.")))

(deftest history-append
  (with-isolated-data
    (xmpp-cli/history:append-history :profile "default"
                                     :to "friend@example.org"
                                     :kind :text
                                     :bytes 5
                                     :sha256 "abc"
                                     :result :sent)
    (xmpp-cli/history:append-history :profile "default"
                                     :to "friend@example.org"
                                     :kind :text
                                     :bytes 5
                                     :sha256 "def"
                                     :result :failed
                                     :error "no route")
    (let* ((text (xmpp-cli/util:read-file-as-string
                  (xmpp-cli/history:history-pathname)))
           (history (read-from-string text)))
      (check-equal 2 (length history))
      (check (not (search "secret message body" text))
             "history should not contain message bodies"))))

(deftest history-reader-eval-disabled
  (with-isolated-data
    (setf *reader-eval-ran* nil)
    (xmpp-cli/util:write-private-file
     (xmpp-cli/history:history-pathname)
     (reader-eval-payload "nil"))
    (check-signals-error
      (xmpp-cli/history::load-history))
    (check (not *reader-eval-ran*)
           "history.sexp reader eval should stay disabled.")))
