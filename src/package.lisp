(defpackage #:xmpp-cli/util
  (:use #:cl)
  (:export
   #:home-xmpp-cli-directory
   #:ensure-private-directory
   #:write-private-file
   #:read-file-as-string
   #:read-stream-as-string
   #:display-path
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

(defpackage #:xmpp-cli/json
  (:use #:cl)
  (:export
   #:parse-json
   #:json-value
   #:json-null
   #:json-null-p
   #:json-compact-string))

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
   #:load-active-routes
   #:save-routes
   #:ensure-route
   #:find-route-by-code
   #:find-active-route-by-code
   #:route-expired-p
   #:mark-route-used))

(defpackage #:xmpp-cli/tmux
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:write-private-file
                #:read-file-as-string)
  (:export
   #:capture-context
   #:context-available-p
   #:focus-pane
   #:paste-text-and-enter))

(defpackage #:xmpp-cli/agent-codex
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:display-path
                #:read-stream-as-string)
  (:import-from #:xmpp-cli/json
                #:parse-json
                #:json-value
                #:json-null-p
                #:json-compact-string)
  (:import-from #:xmpp-cli/agent-config
                #:notify-to)
  (:import-from #:xmpp-cli/agent-routes
                #:canonical-route-identity
                #:ensure-route)
  (:import-from #:xmpp-cli/tmux
                #:capture-context
                #:context-available-p)
  (:export
   #:read-codex-payload
   #:build-codex-notification
   #:notification-target
   #:notification-body
   #:notification-bodies
   #:notification-route))

(defpackage #:xmpp-cli/backend
  (:use #:cl)
  (:export
   #:send-text
   #:check-login
   #:call-with-connection
   #:send-connected-text
   #:receive-connected-message-loop
   #:close-connection))

(defpackage #:xmpp-cli/agent-ipc
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:ensure-private-directory
                #:now-iso8601)
  (:import-from #:xmpp-cli/yaml
                #:emit-yaml
                #:read-yaml-file
                #:write-yaml-file
                #:yaml-value)
  (:import-from #:xmpp-cli/agent-config
                #:agent-directory)
  (:export
   #:control-pathname
   #:daemon-lock-pathname
   #:load-control
   #:load-daemon-lock
   #:save-control
   #:delete-control
   #:acquire-daemon-lock
   #:release-daemon-lock
   #:delete-stale-daemon-lock
   #:make-control-token
   #:read-ipc-message
   #:write-ipc-message
   #:make-ipc-stream
   #:request-control
   #:daemon-send
   #:daemon-status
   #:daemon-stop))

(defpackage #:xmpp-cli/agent-daemon
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:now-iso8601)
  (:import-from #:xmpp-cli/state
                #:load-config
                #:profile)
  (:import-from #:xmpp-cli/agent-config
                #:load-agent-config
                #:allowed-senders)
  (:import-from #:xmpp-cli/agent-routes
                #:load-active-routes
                #:find-active-route-by-code
                #:mark-route-used)
  (:import-from #:xmpp-cli/tmux
                #:focus-pane
                #:paste-text-and-enter)
  (:import-from #:xmpp-cli/backend
                #:call-with-connection
                #:send-connected-text
                #:receive-connected-message-loop
                #:close-connection)
  (:import-from #:xmpp-cli/agent-ipc
                #:load-control
                #:load-daemon-lock
                #:daemon-lock-pathname
                #:save-control
                #:delete-control
                #:acquire-daemon-lock
                #:release-daemon-lock
                #:delete-stale-daemon-lock
                #:make-control-token
                #:read-ipc-message
                #:write-ipc-message
                #:make-ipc-stream
                #:daemon-status)
  (:export
   #:run-daemon
   #:parse-agent-reply
   #:bare-jid))

(defpackage #:xmpp-cli/backend/cl-xmpp
  (:use #:cl)
  (:import-from #:xmpp-cli/backend
                #:send-text
                #:check-login
                #:call-with-connection
                #:send-connected-text
                #:receive-connected-message-loop
                #:close-connection)
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
                #:notify-to
                #:set-notify-to
                #:add-allowed-sender
                #:remove-allowed-sender)
  (:import-from #:xmpp-cli/agent-codex
                #:read-codex-payload
                #:build-codex-notification
                #:notification-target
                #:notification-body
                #:notification-bodies)
  (:import-from #:xmpp-cli/json
                #:json-compact-string)
  (:import-from #:xmpp-cli/yaml
                #:emit-yaml)
  (:import-from #:xmpp-cli/agent-ipc
                #:daemon-send
                #:daemon-status
                #:daemon-stop)
  (:import-from #:xmpp-cli/agent-routes
                #:find-active-route-by-code)
  (:import-from #:xmpp-cli/tmux
                #:focus-pane)
  (:import-from #:xmpp-cli/agent-daemon
                #:run-daemon)
  (:import-from #:xmpp-cli/backend
                #:check-login
                #:send-text)
  (:export
   #:run))

(defpackage #:xmpp-cli/main
  (:use #:cl)
  (:export
   #:entry-point))
