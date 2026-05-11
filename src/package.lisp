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

(defpackage #:xmpp-cli/yaml
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:write-private-file)
  (:export
   #:emit-yaml
   #:parse-yaml
   #:read-yaml-file
   #:write-yaml-file
   #:yaml-value
   #:yaml-null
   #:yaml-null-p))

(defpackage #:xmpp-cli/state
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:write-private-file
                #:split-jid)
  (:import-from #:xmpp-cli/yaml
                #:emit-yaml
                #:read-yaml-file
                #:write-yaml-file
                #:yaml-value
                #:yaml-null-p)
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
  (:import-from #:xmpp-cli/yaml
                #:read-yaml-file
                #:write-yaml-file
                #:yaml-value
                #:yaml-null-p)
  (:export
   #:append-history
   #:history-pathname))

(defpackage #:xmpp-cli/agent-config
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:split-jid)
  (:import-from #:xmpp-cli/yaml
                #:emit-yaml
                #:read-yaml-file
                #:write-yaml-file
                #:yaml-value)
  (:export
   #:agent-directory
   #:agent-config-pathname
   #:load-agent-config
   #:save-agent-config
   #:agent-config-as-yaml
   #:notify-to
   #:set-notify-to
   #:allowed-senders
   #:add-allowed-sender
   #:remove-allowed-sender))

(defpackage #:xmpp-cli/agent-routes
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:now-iso8601
                #:sha256-hex)
  (:import-from #:xmpp-cli/yaml
                #:read-yaml-file
                #:write-yaml-file
                #:yaml-value
                #:yaml-null
                #:yaml-null-p)
  (:export
   #:routes-pathname
   #:canonical-route-identity
   #:route-id-for-identity
   #:random-route-code
   #:load-routes
   #:save-routes
   #:ensure-route
   #:find-route-by-code))

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
  (:import-from #:xmpp-cli/agent-config
                #:load-agent-config
                #:save-agent-config
                #:agent-config-as-yaml
                #:set-notify-to
                #:add-allowed-sender
                #:remove-allowed-sender)
  (:import-from #:xmpp-cli/yaml
                #:emit-yaml)
  (:import-from #:xmpp-cli/backend
                #:check-login
                #:send-text)
  (:export
   #:run))

(defpackage #:xmpp-cli/main
  (:use #:cl)
  (:export
   #:entry-point))
