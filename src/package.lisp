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
   #:parse-iso8601
   #:current-process-id
   #:process-exists-p
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
   #:yaml-null-p
   #:yaml-false
   #:yaml-false-p))

(defpackage #:xmpp-cli/persistence
  (:use #:cl)
  (:import-from #:xmpp-cli/yaml
                #:read-yaml-file
                #:write-yaml-file
                #:yaml-null
                #:yaml-null-p)
  (:export
   #:proper-plist-p
   #:keyword-to-yaml-key
   #:yaml-key-to-keyword
   #:plist-to-yaml
   #:yaml-to-plist
   #:yaml-null-to-nil
   #:nil-to-yaml-null
   #:read-yaml-record-list-file
   #:write-yaml-record-list-file
   #:temporary-sibling-pathname
   #:write-yaml-atomically))

(defpackage #:xmpp-cli/file-lock
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:now-iso8601
                #:current-process-id
                #:process-exists-p)
  (:import-from #:xmpp-cli/yaml
                #:emit-yaml
                #:read-yaml-file
                #:yaml-value)
  (:export
   #:load-file-lock
   #:try-acquire-file-lock
   #:file-lock-owned-p
   #:release-file-lock
   #:delete-file-lock
   #:file-lock-stale-p
   #:acquire-file-lock
   #:call-with-file-lock))

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
                #:split-jid)
  (:import-from #:xmpp-cli/yaml
                #:read-yaml-file
                #:yaml-value
                #:yaml-null-p)
  (:import-from #:xmpp-cli/persistence
                #:proper-plist-p
                #:plist-to-yaml
                #:yaml-to-plist
                #:write-yaml-atomically)
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
                #:now-iso8601)
  (:import-from #:xmpp-cli/yaml
                #:yaml-null-p)
  (:import-from #:xmpp-cli/persistence
                #:plist-to-yaml
                #:yaml-to-plist
                #:read-yaml-record-list-file
                #:write-yaml-record-list-file)
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
                #:read-yaml-file
                #:yaml-value
                #:yaml-null
                #:yaml-false)
  (:import-from #:xmpp-cli/persistence
                #:write-yaml-atomically)
  (:export
   #:agent-directory
   #:ensure-agent-directory
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
                #:now-iso8601
                #:parse-iso8601
                #:sha256-hex)
  (:import-from #:xmpp-cli/agent-config
                #:agent-directory
                #:ensure-agent-directory)
  (:import-from #:xmpp-cli/file-lock
                #:call-with-file-lock)
  (:import-from #:xmpp-cli/persistence
                #:plist-to-yaml
                #:yaml-to-plist
                #:yaml-null-to-nil
                #:nil-to-yaml-null
                #:read-yaml-record-list-file
                #:write-yaml-record-list-file)
  (:export
   #:routes-pathname
   #:routes-lock-pathname
   #:canonical-route-identity
   #:route-id-for-identity
   #:random-route-code
   #:load-routes
   #:load-active-routes
   #:save-routes
	   #:ensure-route
	   #:find-route-by-id
	   #:find-route-by-code
   #:find-active-route-by-code
   #:last-active-route
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
	   #:pane-exists-p
	   #:focus-pane
   #:paste-text-and-enter
   #:start-codex-session))

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
   #:receive-connected-stanza
	   #:receive-connected-message-loop
	   #:send-disco-info
	   #:send-disco-items
	   #:join-room
	   #:request-room-config
	   #:submit-room-config
	   #:grant-room-membership
	   #:send-direct-room-invite
	   #:send-room-message
	   #:destroy-room
	   #:leave-room
	   #:close-connection))

(defpackage #:xmpp-cli/agent-muc
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:now-iso8601)
  (:import-from #:xmpp-cli/yaml
                #:yaml-value)
  (:import-from #:xmpp-cli/agent-config
                #:agent-directory
                #:ensure-agent-directory)
  (:import-from #:xmpp-cli/persistence
                #:read-yaml-record-list-file
                #:write-yaml-record-list-file)
  (:export
   #:+muc-feature+
   #:+disco-info-xmlns+
   #:+disco-items-xmlns+
   #:muc-services-pathname
   #:load-muc-service-cache
   #:save-muc-service-cache
   #:cache-muc-service
   #:muc-service-candidate-p
   #:conference-text-candidate-p
   #:select-muc-service
   #:resolve-muc-service
   #:muc-service-result-summary))

(defpackage #:xmpp-cli/agent-rooms
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:now-iso8601
                #:parse-iso8601)
  (:import-from #:xmpp-cli/agent-config
                #:agent-directory
                #:ensure-agent-directory)
  (:import-from #:xmpp-cli/file-lock
                #:call-with-file-lock)
  (:import-from #:xmpp-cli/persistence
                #:plist-to-yaml
                #:yaml-to-plist
                #:yaml-null-to-nil
                #:nil-to-yaml-null
                #:read-yaml-record-list-file
                #:write-yaml-record-list-file)
  (:export
   #:rooms-pathname
   #:load-rooms
   #:save-rooms
   #:active-room-p
   #:active-rooms
   #:find-room-by-jid
   #:find-active-room-by-jid
   #:find-active-room-by-route-code
   #:find-active-room-by-route-id
   #:room-expired-p
   #:upsert-room
   #:mark-room-activity
   #:mark-room-closed
   #:room-bare-jid
   #:room-nick
   #:room-full-jid
   #:parse-room-occupant-jid
   #:sanitize-room-slug
   #:make-room-node
   #:make-room-jid
   #:join-uri
   #:room-summary-lines))

(defpackage #:xmpp-cli/agent-ipc
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:ensure-private-directory
                #:sha256-hex
                #:utf-8-octets)
  (:import-from #:xmpp-cli/yaml
                #:read-yaml-file
                #:yaml-value)
  (:import-from #:xmpp-cli/agent-config
                #:agent-directory)
  (:import-from #:xmpp-cli/persistence
                #:write-yaml-atomically)
  (:import-from #:xmpp-cli/file-lock
                #:load-file-lock
                #:try-acquire-file-lock
                #:file-lock-owned-p
                #:release-file-lock
                #:delete-file-lock)
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
   #:profile-digest
   #:read-ipc-message
   #:read-ipc-message-with-timeout
   #:write-ipc-message
   #:make-ipc-stream
   #:request-control
   #:daemon-send
   #:daemon-notify
   #:daemon-status
   #:daemon-stop
   #:wait-for-daemon-stop
   #:daemon-discover-muc))

(defpackage #:xmpp-cli/agent-daemon
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:display-path
                #:now-iso8601
                #:current-process-id
                #:process-exists-p)
  (:import-from #:xmpp-cli/state
                #:load-config
                #:profile)
  (:import-from #:xmpp-cli/agent-config
                #:load-agent-config
                #:allowed-senders)
  (:import-from #:xmpp-cli/agent-routes
                #:canonical-route-identity
                #:ensure-route
                #:load-active-routes
                #:find-route-by-id
                #:find-route-by-code
                #:last-active-route
                #:mark-route-used)
  (:import-from #:xmpp-cli/tmux
                #:focus-pane
                #:pane-exists-p
                #:paste-text-and-enter
                #:start-codex-session)
  (:import-from #:xmpp-cli/backend
                #:call-with-connection
                #:send-connected-text
                #:receive-connected-message-loop
                #:send-disco-info
                #:send-disco-items
                #:join-room
                #:request-room-config
                #:submit-room-config
                #:grant-room-membership
                #:send-direct-room-invite
                #:send-room-message
                #:destroy-room
                #:leave-room
                #:close-connection)
  (:import-from #:xmpp-cli/agent-muc
                #:resolve-muc-service)
  (:import-from #:xmpp-cli/agent-rooms
                #:load-rooms
                #:active-rooms
                #:find-active-room-by-jid
                #:find-active-room-by-route-code
                #:find-active-room-by-route-id
                #:room-expired-p
                #:upsert-room
                #:mark-room-activity
                #:mark-room-closed
                #:room-bare-jid
                #:room-nick
                #:room-full-jid
                #:parse-room-occupant-jid
                #:sanitize-room-slug
                #:make-room-node
                #:make-room-jid
                #:join-uri
                #:room-summary-lines)
  (:import-from #:xmpp-cli/agent-ipc
                #:load-daemon-lock
                #:daemon-lock-pathname
                #:save-control
                #:delete-control
                #:acquire-daemon-lock
                #:release-daemon-lock
                #:delete-stale-daemon-lock
                #:make-control-token
                #:profile-digest
                #:read-ipc-message-with-timeout
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
	                #:receive-connected-stanza
	                #:receive-connected-message-loop
	                #:send-disco-info
	                #:send-disco-items
	                #:join-room
	                #:request-room-config
	                #:submit-room-config
	                #:grant-room-membership
	                #:send-direct-room-invite
	                #:send-room-message
	                #:destroy-room
	                #:leave-room
	                #:close-connection)
  (:import-from #:xmpp-cli/agent-rooms
                #:parse-room-occupant-jid)
  (:export
   #:make-backend))

(defpackage #:xmpp-cli/sender
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:sha256-hex
                #:utf-8-byte-length)
  (:import-from #:xmpp-cli/history
                #:append-history)
  (:import-from #:xmpp-cli/agent-config
                #:load-agent-config)
  (:import-from #:xmpp-cli/agent-ipc
                #:load-control
                #:daemon-send
                #:daemon-notify
                #:profile-digest)
  (:import-from #:xmpp-cli/backend
                #:check-login
                #:send-text)
  (:export
   #:*backend-factory*
   #:make-backend
   #:check-profile-login
   #:append-send-history
   #:maybe-append-send-history
   #:daemon-compatible-profile-p
   #:delivery-target-label
   #:send-message-with-fallback
   #:send-message-parts-with-fallback
   #:send-notification-with-fallback
   #:send-notification-parts-with-fallback))

(defpackage #:xmpp-cli/cli
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:read-file-as-string
                #:split-jid)
  (:import-from #:xmpp-cli/state
                #:*default-profile-name*
                #:load-config
                #:save-config
                #:profile
                #:set-profile
                #:default-profile-name)
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
                #:daemon-status
                #:daemon-stop
                #:wait-for-daemon-stop
                #:daemon-discover-muc)
  (:import-from #:xmpp-cli/agent-muc
                #:muc-service-result-summary)
  (:import-from #:xmpp-cli/sender
                #:make-backend
                #:check-profile-login
                #:maybe-append-send-history
                #:append-send-history
                #:delivery-target-label
                #:send-message-with-fallback
                #:send-message-parts-with-fallback
                #:send-notification-parts-with-fallback)
  (:import-from #:xmpp-cli/agent-routes
                #:find-active-route-by-code)
  (:import-from #:xmpp-cli/tmux
                #:focus-pane)
  (:import-from #:xmpp-cli/agent-daemon
                #:run-daemon)
  (:export
   #:run))

(defpackage #:xmpp-cli/main
  (:use #:cl)
  (:export
   #:entry-point))
