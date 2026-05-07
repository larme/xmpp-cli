(defpackage #:xmpp-cli/util
  (:use #:cl)
  (:export
   #:home-xmpp-cli-directory
   #:ensure-private-directory
   #:write-private-file
   #:read-file-as-string
   #:split-jid
   #:now-iso8601
   #:utf-8-octets
   #:utf-8-byte-length
   #:sha256-hex))

(defpackage #:xmpp-cli/state
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:write-private-file
                #:split-jid)
  (:export
   #:*default-profile-name*
   #:load-config
   #:save-config
   #:profile
   #:set-profile
   #:default-profile-name
   #:config-pathname))

(defpackage #:xmpp-cli/history
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:write-private-file
                #:now-iso8601
                #:sha256-hex
                #:utf-8-byte-length)
  (:export
   #:append-history
   #:history-pathname))

(defpackage #:xmpp-cli/backend
  (:use #:cl)
  (:export
   #:send-text
   #:check-login))

(defpackage #:xmpp-cli/backend/cl-xmpp
  (:use #:cl)
  (:import-from #:xmpp-cli/backend
                #:send-text
                #:check-login)
  (:export
   #:make-backend))

(defpackage #:xmpp-cli/cli
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:read-file-as-string
                #:split-jid
                #:sha256-hex
                #:utf-8-byte-length)
  (:import-from #:xmpp-cli/state
                #:*default-profile-name*
                #:load-config
                #:save-config
                #:profile
                #:set-profile
                #:default-profile-name)
  (:import-from #:xmpp-cli/history
                #:append-history)
  (:import-from #:xmpp-cli/backend
                #:check-login
                #:send-text)
  (:export
   #:run))

(defpackage #:xmpp-cli/main
  (:use #:cl)
  (:export
   #:entry-point))
