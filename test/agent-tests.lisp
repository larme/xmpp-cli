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

(deftest daemon-lock-is-exclusive-and-token-owned
  (with-isolated-data
    (let ((first-token "aaaaaaaa")
          (second-token "bbbbbbbb"))
      (check (xmpp-cli/agent-ipc:acquire-daemon-lock first-token :pid 12345)
             "first daemon should acquire the lock")
      (check-equal 12345
                   (getf (xmpp-cli/agent-ipc:load-daemon-lock) :pid))
      (check (not (xmpp-cli/agent-ipc:acquire-daemon-lock second-token))
             "second daemon should not acquire an existing lock")
      (check (not (xmpp-cli/agent-ipc:release-daemon-lock second-token))
             "non-owner should not release the daemon lock")
      (check (probe-file (xmpp-cli/agent-ipc:daemon-lock-pathname))
             "lock should still exist after non-owner release")
      (check (not (xmpp-cli/agent-ipc:delete-stale-daemon-lock second-token))
             "stale-lock cleanup should not delete another token")
      (check (probe-file (xmpp-cli/agent-ipc:daemon-lock-pathname))
             "lock should still exist after non-owner stale cleanup")
      (check (xmpp-cli/agent-ipc:delete-stale-daemon-lock first-token)
             "stale-lock cleanup should delete the observed token")
      (check (not (probe-file (xmpp-cli/agent-ipc:daemon-lock-pathname)))
             "lock should be removed after owner stale cleanup")
      (check (xmpp-cli/agent-ipc:acquire-daemon-lock first-token)
             "daemon should reacquire the lock after stale cleanup")
      (check (xmpp-cli/agent-ipc:release-daemon-lock first-token)
             "owner should release the daemon lock")
      (check (not (probe-file (xmpp-cli/agent-ipc:daemon-lock-pathname)))
             "lock should be removed after owner release"))))

(deftest daemon-control-delete-is-token-safe
  (with-isolated-data
    (let ((control (list :pid 123
                         :host "127.0.0.1"
                         :port 4567
                         :token "owner-token"
                         :started-at "2026-05-12T00:00:00+08:00"
                         :profile "default")))
      (xmpp-cli/agent-ipc:save-control control)
      (check (not (xmpp-cli/agent-ipc:delete-control "other-token"))
             "non-owner should not delete control.yaml")
      (check (probe-file (xmpp-cli/agent-ipc:control-pathname))
             "control.yaml should remain after non-owner delete")
      (check (xmpp-cli/agent-ipc:delete-control "owner-token")
             "owner should delete control.yaml")
      (check (not (probe-file (xmpp-cli/agent-ipc:control-pathname)))
             "control.yaml should be removed by owner"))))

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

(deftest stream-and-yaml-preserve-unicode
  (let* ((text (format nil "I~Cm fine" (code-char #x2019)))
         (yaml (xmpp-cli/yaml:emit-yaml
                (list (cons "message" text)))))
    (check-equal text
                 (with-input-from-string (in text)
                   (xmpp-cli/util:read-stream-as-string in)))
    (check-equal text
                 (xmpp-cli/yaml:yaml-value
                  (xmpp-cli/yaml:parse-yaml yaml)
                  "message"))))

(deftest tmux-format-strings-use-real-tabs
  (check (search (string #\Tab) xmpp-cli/tmux::*tmux-display-format*)
         "tmux display format should contain real tab separators")
  (check (search (string #\Tab) xmpp-cli/tmux::*tmux-pane-location-format*)
         "tmux pane location format should contain real tab separators")
  (check (search (string #\Tab) xmpp-cli/tmux::*tmux-client-format*)
         "tmux client format should contain real tab separators"))

(deftest tmux-display-line-parses-stable-ids
  (let ((context (xmpp-cli/tmux::parse-tmux-display-line
                  "/dev/pts/45	/dev/pts/45	$1	session	@3	0	editor	%12	1	/home/larme/codes/cl-projects/xmpp-cli
"
                  "/tmp/tmux-1000/default,123,0")))
    (check-equal "/tmp/tmux-1000/default" (getf context :tmux-socket))
    (check-equal "/dev/pts/45" (getf context :tmux-client-name))
    (check-equal "/dev/pts/45" (getf context :tmux-client-tty))
    (check-equal "$1" (getf context :tmux-session-id))
    (check-equal "@3" (getf context :tmux-window-id))
    (check-equal "%12" (getf context :tmux-pane-id))))

(deftest tmux-fallback-context-uses-env-pane
  (let ((context (xmpp-cli/tmux::fallback-tmux-context
                  "/tmp/tmux-1000/default,123,0"
                  "%57")))
    (check-equal "/tmp/tmux-1000/default" (getf context :tmux-socket))
    (check-equal "%57" (getf context :tmux-pane-id))))

(deftest tmux-pane-location-parses-window-id
  (let ((location (xmpp-cli/tmux::parse-tmux-pane-location-line
                   "/dev/pts/45	/dev/pts/45	$6	@98
")))
    (check-equal "/dev/pts/45" (getf location :tmux-client-name))
    (check-equal "$6" (getf location :tmux-session-id))
    (check-equal "@98" (getf location :tmux-window-id))))

(deftest tmux-client-line-parses-client-name
  (let ((client (xmpp-cli/tmux::parse-tmux-client-line
                 "/dev/pts/45	$6
")))
    (check-equal "/dev/pts/45" (getf client :tmux-client-name))
    (check-equal "$6" (getf client :tmux-session-id))))

(deftest agent-reply-parser-is-case-insensitive
  (multiple-value-bind (code text)
      (xmpp-cli/agent-daemon:parse-agent-reply
       "  AbCd please rerun the failing test  ")
    (check-equal "abcd" code)
    (check-equal "please rerun the failing test" text))
  (multiple-value-bind (code text)
      (xmpp-cli/agent-daemon:parse-agent-reply "ABCD")
    (check-equal "abcd" code)
    (check-equal "" text))
  (check-equal "user@example.org"
               (xmpp-cli/agent-daemon:bare-jid
                "user@example.org/phone")))

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
                          :tmux-client-name "/dev/pts/45"
                          :tmux-client-tty "/dev/pts/45"
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
      (check-equal "/dev/pts/45" (getf route-a :tmux-client-name))
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

(deftest cli-agent-notify-codex-sends-unicode-message
  (with-isolated-data
    (save-default-test-profile)
    (xmpp-cli/agent-config:save-agent-config
     (xmpp-cli/agent-config:set-notify-to
      (xmpp-cli/agent-config:load-agent-config)
      "friend@example.org"))
    (let ((payload (format nil
                           "{\"hook_event_name\":\"Stop\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"cwd\":\"/tmp\",\"last_assistant_message\":\"I~Cm fine\"}"
                           (code-char #x2019))))
      (multiple-value-bind (code events output error-output)
          (run-cli '("agent" "notify-codex") :input payload)
        (declare (ignore output error-output))
        (check-equal 0 code)
        (check-equal 1 (length events))
        (destructuring-bind (event profile to body) (first events)
          (declare (ignore profile to))
          (check-equal :send-text event)
          (check (search (format nil "I~Cm fine" (code-char #x2019)) body)
                 "notification body should preserve Unicode punctuation"))))))

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
