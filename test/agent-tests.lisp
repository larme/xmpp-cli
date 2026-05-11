(in-package #:xmpp-cli/test)

(deftest agent-config-defaults-and-save
  (with-isolated-data
    (let ((config (xmpp-cli/agent-config:load-agent-config)))
      (check-equal "default" (getf config :profile))
      (check-equal '(1 2 5 10 30 60 300)
                   (getf config :reconnect-backoff-seconds))
      (let ((updated (xmpp-cli/agent-config:set-notify-to
                      config
                      "user@example.org")))
        (xmpp-cli/agent-config:save-agent-config updated)
        (let* ((text (xmpp-cli/util:read-file-as-string
                      (xmpp-cli/agent-config:agent-config-pathname)))
               (loaded (xmpp-cli/agent-config:load-agent-config)))
          (check (search "notify_to" text)
                 "agent config should be written as YAML")
          (check-equal "user@example.org"
                       (getf loaded :notify-to))
          (check-equal '("user@example.org")
                       (getf loaded :allowed-senders)))))))

(deftest route-code-generation-is-lowercase
  (loop repeat 100
        for code = (xmpp-cli/agent-routes:random-route-code 4)
        do (progn
             (check-equal 4 (length code))
             (check (every (lambda (char)
                             (and (char>= char #\a)
                                  (char<= char #\z)))
                           code)
                    "route code should be lowercase letters: ~a"
                    code))))

(deftest routes-reuse-code-and-match-case-insensitively
  (with-isolated-data
    (let* ((identity (xmpp-cli/agent-routes:canonical-route-identity
                      :host "hbox"
                      :tmux-socket "/tmp/tmux-1000/default"
                      :tmux-session-id "$1"
                      :tmux-window-id "@3"
                      :tmux-pane-id "%12"
                      :agent :codex
                      :agent-session "codex-session"))
           (route-a (xmpp-cli/agent-routes:ensure-route
                     identity
                     :agent :codex
                     :agent-session "codex-session"
                     :host "hbox"
                     :cwd "/home/larme/codes/cl-projects/xmpp-cli"
                     :display-cwd "~/codes/cl-projects/xmpp-cli"
                     :tmux-socket "/tmp/tmux-1000/default"
                     :tmux-session-id "$1"
                     :tmux-window-id "@3"
                     :tmux-pane-id "%12"))
           (code (getf route-a :code))
           (route-b (xmpp-cli/agent-routes:ensure-route
                     identity
                     :agent :codex
                     :agent-session "codex-session"
                     :host "hbox"
                     :cwd "/home/larme/codes/cl-projects/xmpp-cli"
                     :display-cwd "~/codes/cl-projects/xmpp-cli"
                     :tmux-socket "/tmp/tmux-1000/default"
                     :tmux-session-id "$1"
                     :tmux-window-id "@3"
                     :tmux-pane-id "%12"))
           (matched (xmpp-cli/agent-routes:find-route-by-code
                     (string-upcase code))))
      (check-equal code (getf route-b :code))
      (check-equal code (getf matched :code))
      (check-equal 2 (getf route-b :notify-count)))))

(deftest agent-config-cli-set-notify-to
  (with-isolated-data
    (multiple-value-bind (code events output error-output)
        (run-cli '("agent" "config" "set-notify-to" "user@example.org"))
      (declare (ignore events error-output))
      (check-equal 0 code)
      (check (search "user@example.org" output)
             "CLI should report configured JID")
      (let ((config (xmpp-cli/agent-config:load-agent-config)))
        (check-equal "user@example.org" (getf config :notify-to))))))

(deftest agent-config-empty-allowed-senders-stays-empty
  (with-isolated-data
    (let* ((config (xmpp-cli/agent-config:set-notify-to
                    (xmpp-cli/agent-config:load-agent-config)
                    "user@example.org"))
           (without-sender (xmpp-cli/agent-config:remove-allowed-sender
                            config
                            "user@example.org")))
      (xmpp-cli/agent-config:save-agent-config without-sender)
      (let ((loaded (xmpp-cli/agent-config:load-agent-config)))
        (check-equal "user@example.org" (getf loaded :notify-to))
        (check-equal nil (getf loaded :allowed-senders))))))

(deftest json-parser-basic-object
  (let ((payload (xmpp-cli/json:parse-json
                  "{\"hook_event_name\":\"Stop\",\"turn_id\":\"t1\",\"tool_input\":{\"command\":\"ls\"},\"items\":[1,true,null]}")))
    (check-equal "Stop" (xmpp-cli/json:json-value payload "hook_event_name"))
    (check-equal "ls"
                 (xmpp-cli/json:json-value
                  (xmpp-cli/json:json-value payload "tool_input")
                  "command"))
    (check (xmpp-cli/json:json-null-p
            (third (xmpp-cli/json:json-value payload "items")))
           "JSON null should round-trip as the internal null marker.")))

(deftest tmux-display-line-parses-stable-ids
  (let ((context (xmpp-cli/tmux::parse-tmux-display-line
                  "$1	session	@3	0	editor	%12	1	/home/larme/codes/cl-projects/xmpp-cli
"
                  "/tmp/tmux-1000/default,123,0")))
    (check-equal "/tmp/tmux-1000/default" (getf context :tmux-socket))
    (check-equal "$1" (getf context :tmux-session-id))
    (check-equal "@3" (getf context :tmux-window-id))
    (check-equal "%12" (getf context :tmux-pane-id))))

(deftest tmux-fallback-context-uses-env-pane
  (let ((context (xmpp-cli/tmux::fallback-tmux-context
                  "/tmp/tmux-1000/default,123,0"
                  "%57")))
    (check-equal "/tmp/tmux-1000/default" (getf context :tmux-socket))
    (check-equal "%57" (getf context :tmux-pane-id))))

(deftest codex-notification-allocates-route-code
  (with-isolated-data
    (let* ((cwd (namestring (uiop:getcwd)))
           (config (xmpp-cli/agent-config:set-notify-to
                    (xmpp-cli/agent-config:load-agent-config)
                    "friend@example.org"))
           (payload (xmpp-cli/json:parse-json
                     (format nil
                             "{\"hook_event_name\":\"Stop\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"session_id\":\"session-1\",\"cwd\":\"~a\",\"last_assistant_message\":\"done\"}"
                             cwd)))
           (context (list :tmux-socket "/tmp/tmux-1000/default"
                          :tmux-session-id "$1"
                          :tmux-window-id "@3"
                          :tmux-pane-id "%12"))
           (notification-a
             (xmpp-cli/agent-codex:build-codex-notification
              payload
              config
              :tmux-context context
              :host "hbox"))
           (route-a (xmpp-cli/agent-codex:notification-route notification-a))
           (code (getf route-a :code))
           (notification-b
             (xmpp-cli/agent-codex:build-codex-notification
              payload
              config
              :tmux-context context
              :host "hbox"))
           (route-b (xmpp-cli/agent-codex:notification-route notification-b)))
      (check-equal "friend@example.org"
                   (xmpp-cli/agent-codex:notification-target notification-a))
      (check-equal 4 (length code))
      (check (every (lambda (char)
                      (and (char>= char #\a)
                           (char<= char #\z)))
                    code)
             "route code should be lowercase letters")
      (check (search (format nil "~a hbox " code)
                     (xmpp-cli/agent-codex:notification-body notification-a))
             "notification body should start with the route code and host")
      (check-equal code (getf route-b :code)))))

(deftest codex-notification-routes-with-env-only-tmux-context
  (with-isolated-data
    (let* ((config (xmpp-cli/agent-config:set-notify-to
                    (xmpp-cli/agent-config:load-agent-config)
                    "friend@example.org"))
           (payload (xmpp-cli/json:parse-json
                     "{\"hook_event_name\":\"Stop\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"session_id\":\"session-1\",\"cwd\":\"/tmp\",\"last_assistant_message\":\"done\"}"))
           (context (list :tmux-socket "/tmp/tmux-1000/default"
                          :tmux-pane-id "%57"))
           (notification
             (xmpp-cli/agent-codex:build-codex-notification
              payload
              config
              :tmux-context context
              :host nil))
           (route (xmpp-cli/agent-codex:notification-route notification))
           (body (xmpp-cli/agent-codex:notification-body notification)))
      (check-equal 4 (length (getf route :code)))
      (check (not (search "no-route" body))
             "notification should allocate a route from TMUX/TMUX_PANE fallback context")
      (check (not (search " NIL " body))
             "notification header should not contain printed NIL host"))))

(deftest cli-agent-notify-codex-sends-with-fake-backend
  (with-isolated-data
    (save-default-test-profile)
    (xmpp-cli/agent-config:save-agent-config
     (xmpp-cli/agent-config:set-notify-to
      (xmpp-cli/agent-config:load-agent-config)
      "friend@example.org"))
    (multiple-value-bind (code events output error-output)
        (run-cli '("agent" "notify-codex")
                 :input "{\"hook_event_name\":\"Stop\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"cwd\":\"/tmp\",\"last_assistant_message\":\"done\"}")
      (declare (ignore error-output))
      (check-equal 0 code)
      (check-equal "" output)
      (check-equal 1 (length events))
      (destructuring-bind (event profile to body) (first events)
        (declare (ignore profile))
        (check-equal :send-text event)
        (check-equal "friend@example.org" to)
        (check (search "codex finished" body)
               "notification body should summarize the Codex event")))))

(deftest cli-agent-notify-codex-missing-profile-is-best-effort
  (with-isolated-data
    (xmpp-cli/agent-config:save-agent-config
     (xmpp-cli/agent-config:set-notify-to
      (xmpp-cli/agent-config:load-agent-config)
      "friend@example.org"))
    (multiple-value-bind (code events output error-output)
        (run-cli '("agent" "notify-codex")
                 :input "{\"hook_event_name\":\"Stop\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"cwd\":\"/tmp\",\"last_assistant_message\":\"done\"}")
      (declare (ignore error-output))
      (check-equal 0 code)
      (check-equal nil events)
      (let ((response (xmpp-cli/json:parse-json output)))
        (check (search "no auth/profile data found"
                       (xmpp-cli/json:json-value response "systemMessage"))
               "notify-codex should return valid hook JSON when login is missing")))))
