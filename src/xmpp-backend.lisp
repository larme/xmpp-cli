(in-package #:xmpp-cli/backend)

(defgeneric check-login (backend profile)
  (:documentation "Return true if PROFILE can authenticate successfully."))

(defgeneric send-text (backend profile to body)
  (:documentation "Send BODY as a chat message to TO using PROFILE."))

(defgeneric call-with-connection (backend profile function &key resource send-presence)
  (:documentation "Call FUNCTION with a connected backend-specific XMPP connection."))

(defgeneric send-connected-text (backend connection to body)
  (:documentation "Send BODY as a chat message to TO using an existing CONNECTION."))

(defgeneric receive-connected-message-loop (backend connection handler)
  (:documentation "Receive messages on CONNECTION and call HANDLER with message plists."))

(defgeneric close-connection (backend connection)
  (:documentation "Close a backend-specific XMPP connection."))
