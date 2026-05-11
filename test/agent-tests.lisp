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
