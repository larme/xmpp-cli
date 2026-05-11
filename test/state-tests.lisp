(in-package #:xmpp-cli/test)

(defvar *reader-eval-ran* nil)

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

(deftest config-is-yaml-not-reader-syntax
  (with-isolated-data
    (setf *reader-eval-ran* nil)
    (xmpp-cli/util:write-private-file
     (xmpp-cli/state:config-pathname)
     (format nil
             "default_profile: \"#.(setf xmpp-cli/test::*reader-eval-ran* t)\"~%profiles: []~%"))
    (let ((config (xmpp-cli/state:load-config)))
      (check-equal "#.(setf xmpp-cli/test::*reader-eval-ran* t)"
                   (xmpp-cli/state:default-profile-name config)))
    (check (not *reader-eval-ran*)
           "config.yaml must not evaluate Lisp reader syntax.")))

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
           (history (xmpp-cli/history::load-history)))
      (check-equal 2 (length history))
      (check (not (search "secret message body" text))
             "history should not contain message bodies"))))

(deftest history-is-yaml-not-reader-syntax
  (with-isolated-data
    (setf *reader-eval-ran* nil)
    (xmpp-cli/util:write-private-file
     (xmpp-cli/history:history-pathname)
     (format nil
             "entries:~%  - time: \"#.(setf xmpp-cli/test::*reader-eval-ran* t)\"~%    profile: \"default\"~%"))
    (let ((history (xmpp-cli/history::load-history)))
      (check-equal "#.(setf xmpp-cli/test::*reader-eval-ran* t)"
                   (getf (first history) :time)))
    (check (not *reader-eval-ran*)
           "history.yaml must not evaluate Lisp reader syntax.")))
