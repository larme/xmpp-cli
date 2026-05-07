(in-package #:xmpp-cli/backend)

(defgeneric check-login (backend profile)
  (:documentation "Return true if PROFILE can authenticate successfully."))

(defgeneric send-text (backend profile to body)
  (:documentation "Send BODY as a chat message to TO using PROFILE."))
