(in-package #:xmpp-cli/backend)

(defgeneric check-login (backend profile)
  (:documentation "Return true if PROFILE can authenticate successfully."))

(defgeneric send-text (backend profile to body)
  (:documentation "Send BODY as a chat message to TO using PROFILE."))

(defgeneric call-with-connection (backend profile function &key resource send-presence)
  (:documentation "Call FUNCTION with a connected backend-specific XMPP connection."))

(defgeneric send-connected-text (backend connection to body)
  (:documentation "Send BODY as a chat message to TO using an existing CONNECTION."))

(defgeneric receive-connected-stanza (backend connection)
  (:documentation "Receive one stanza on CONNECTION and return a normalized event plist."))

(defgeneric receive-connected-message-loop (backend connection handler)
  (:documentation "Receive stanzas on CONNECTION and call HANDLER with event plists."))

(defgeneric send-disco-info (backend connection to id &key node)
  (:documentation "Send a disco#info IQ request on CONNECTION."))

(defgeneric send-disco-items (backend connection to id &key node)
  (:documentation "Send a disco#items IQ request on CONNECTION."))

(defgeneric join-room (backend connection room-full-jid)
  (:documentation "Join ROOM-FULL-JID using MUC presence."))

(defgeneric request-room-config (backend connection room-jid id)
  (:documentation "Request a MUC room configuration form."))

(defgeneric submit-room-config (backend connection room-jid id fields)
  (:documentation "Submit MUC room configuration FIELDS."))

(defgeneric grant-room-membership (backend connection room-jid id jid)
  (:documentation "Grant member affiliation for JID in ROOM-JID."))

(defgeneric send-direct-room-invite (backend connection to room-jid reason)
  (:documentation "Send a direct MUC invitation to TO."))

(defgeneric send-room-message (backend connection room-jid body)
  (:documentation "Send BODY as a groupchat message to ROOM-JID."))

(defgeneric destroy-room (backend connection room-jid id &key reason)
  (:documentation "Destroy ROOM-JID with a MUC owner IQ."))

(defgeneric leave-room (backend connection room-full-jid)
  (:documentation "Leave ROOM-FULL-JID using unavailable presence."))

(defgeneric close-connection (backend connection)
  (:documentation "Close a backend-specific XMPP connection."))
