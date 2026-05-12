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

(deftest route-ttl-prunes-expired-routes
  (with-isolated-data
    (let* ((now (get-universal-time))
           (old-time (xmpp-cli/util:now-iso8601 (- now (* 2 24 60 60))))
           (fresh-time (xmpp-cli/util:now-iso8601 now))
           (old-route (list :route-id "old-route"
                            :code "oldc"
                            :identity "old"
                            :created-at old-time
                            :last-seen-at old-time
                            :last-used-at nil))
           (fresh-route (list :route-id "fresh-route"
                              :code "newc"
                              :identity "fresh"
                              :created-at old-time
                              :last-seen-at old-time
                              :last-used-at fresh-time)))
      (xmpp-cli/agent-routes:save-routes (list old-route fresh-route))
      (check (null (xmpp-cli/agent-routes:find-active-route-by-code
                    "oldc"
                    1))
             "expired route codes should not match")
      (let ((matched (xmpp-cli/agent-routes:find-active-route-by-code
                      "NEWC"
                      1)))
        (check-equal "newc" (getf matched :code)))
      (check-equal '("newc")
                   (mapcar (lambda (route)
                             (getf route :code))
                           (xmpp-cli/agent-routes:load-routes))))))

(deftest route-lock-is-exclusive-and-temp-paths-are-unique
  (with-isolated-data
    (let ((temp-a (xmpp-cli/agent-routes::routes-temp-pathname))
          (temp-b (xmpp-cli/agent-routes::routes-temp-pathname))
          (token-a (xmpp-cli/agent-routes::make-routes-lock-token))
          (token-b (xmpp-cli/agent-routes::make-routes-lock-token)))
      (check (not (equal (namestring temp-a) (namestring temp-b)))
             "route temp paths should be unique")
      (check (xmpp-cli/agent-routes::acquire-routes-lock token-a)
             "first route lock acquire should succeed")
      (check-signals-error
        (xmpp-cli/agent-routes::acquire-routes-lock token-b
                                                    :timeout-seconds 0))
      (check (not (xmpp-cli/agent-routes::release-routes-lock token-b))
             "non-owner should not release route lock")
      (check (probe-file (xmpp-cli/agent-routes:routes-lock-pathname))
             "route lock should remain after non-owner release")
      (check (xmpp-cli/agent-routes::release-routes-lock token-a)
             "owner should release route lock")
      (check (not (probe-file (xmpp-cli/agent-routes:routes-lock-pathname)))
             "route lock should be removed after owner release"))))

(deftest route-lock-keeps-live-owner-past-stale-age
  (with-isolated-data
    (let ((token-a (xmpp-cli/agent-routes::make-routes-lock-token))
          (token-b (xmpp-cli/agent-routes::make-routes-lock-token)))
      (unwind-protect
           (progn
             (check (xmpp-cli/agent-routes::acquire-routes-lock token-a)
                    "first route lock acquire should succeed")
             (let ((lock (xmpp-cli/agent-routes::load-routes-lock)))
               (check (xmpp-cli/util:process-exists-p (getf lock :pid))
                      "route lock owner pid should identify a live process"))
             (let ((xmpp-cli/agent-routes::*routes-lock-stale-seconds* -1))
               (check-signals-error
                 (xmpp-cli/agent-routes::acquire-routes-lock
                  token-b
                  :timeout-seconds 0)))
             (check (xmpp-cli/agent-routes::routes-lock-owned-p token-a)
                    "age alone should not steal a live owner's route lock"))
        (xmpp-cli/agent-routes::release-routes-lock token-a)))))

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

(deftest daemon-request-stop-wakes-control-accept-loop
  (let* ((server (usocket:socket-listen "127.0.0.1"
                                        0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (state (xmpp-cli/agent-daemon::make-daemon-state
                 :server-socket server))
         (thread nil))
    (unwind-protect
         (progn
           (setf thread
                 (bt:make-thread
                  (lambda ()
                    (xmpp-cli/agent-daemon::accept-control-loop state))
                  :name "xmpp-cli test accept loop"))
           (sleep 0.1)
           (xmpp-cli/agent-daemon::request-stop state)
           (loop repeat 30
                 while (bt:thread-alive-p thread)
                 do (sleep 0.1))
           (check (not (bt:thread-alive-p thread))
                  "request-stop should close the server socket and wake accept"))
      (ignore-errors
        (usocket:socket-close server))
      (when (and thread (bt:thread-alive-p thread))
        (ignore-errors
          (bt:destroy-thread thread))))))

(deftest daemon-ipc-frame-round-trips-newline-payload
  (let* ((body (format nil "hello~%world I~cm fine" (code-char #x2019)))
         (message (list :op :send
                        :token "owner-token"
                        :body body))
         (out (flexi-streams:make-in-memory-output-stream)))
    (xmpp-cli/agent-ipc:write-ipc-message out message)
    (let* ((wire (flexi-streams:get-output-stream-sequence out))
           (newline (position 10 wire))
           (payload-length (- (length wire) newline 1))
           (line (map 'string #'code-char (subseq wire 0 newline)))
           (in (flexi-streams:make-in-memory-input-stream wire)))
      (check-equal payload-length (parse-integer line))
      (check-equal message
                   (xmpp-cli/agent-ipc:read-ipc-message in)))))

(deftest daemon-ipc-rejects-invalid-frame-lengths
  (check-signals-error
    (xmpp-cli/agent-ipc::parse-ipc-length "-1"))
  (check-signals-error
    (xmpp-cli/agent-ipc::parse-ipc-length
     (princ-to-string
      (1+ xmpp-cli/agent-ipc::*max-ipc-frame-octets*))))
  (let* ((line (format nil "~d~%" (1+ xmpp-cli/agent-ipc::*max-ipc-frame-octets*)))
         (octets (xmpp-cli/util:utf-8-octets line))
         (in (flexi-streams:make-in-memory-input-stream octets)))
    (check-signals-error
      (xmpp-cli/agent-ipc:read-ipc-message in))))

(deftest daemon-send-rejects-mismatched-profile-digest
  (let* ((profile (list :jid "user@example.org"
                        :password "secret"))
         (digest (xmpp-cli/agent-ipc:profile-digest profile))
         (state (xmpp-cli/agent-daemon::make-daemon-state
                 :profile-name "default"
                 :profile profile
                 :control (list :profile-digest digest)
                 :token "owner-token")))
    (dolist (expected '(nil "other-digest"))
      (let ((response
              (xmpp-cli/agent-daemon::handle-control-request
               state
               (list :token "owner-token"
                     :op :send
                     :to "friend@example.org"
                     :body "hello"
                     :expected-profile-digest expected))))
        (check (not (getf response :ok))
               "mismatched daemon profile digest should reject send")
        (check (search "profile" (getf response :error))
               "profile digest mismatch should explain the rejection")))
    (let ((response
            (xmpp-cli/agent-daemon::handle-control-request
             state
             (list :token "owner-token"
                   :op :send
                   :to "friend@example.org"
                   :body "hello"
                   :expected-profile-digest digest))))
      (check (search "connection is not ready" (getf response :error))
             "matching digest should reach the normal send path"))))

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

(deftest agent-reply-without-code-uses-last-active-route
  (with-isolated-data
    (let* ((older (xmpp-cli/util:now-iso8601 (- (get-universal-time) 60)))
           (newer (xmpp-cli/util:now-iso8601))
           (route-a (list :route-id "route-a"
                          :code "aaaa"
                          :identity "route-a"
                          :created-at older
                          :last-seen-at older
                          :last-used-at nil))
           (route-b (list :route-id "route-b"
                          :code "bbbb"
                          :identity "route-b"
                          :created-at older
                          :last-seen-at newer
                          :last-used-at nil))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :agent-config (list :route-ttl-days 90))))
      (xmpp-cli/agent-routes:save-routes (list route-a route-b))
      (multiple-value-bind (route text default-route-p)
          (xmpp-cli/agent-daemon::resolve-route-reply
           state
           "please rerun the failing test")
        (check-equal "bbbb" (getf route :code))
        (check-equal "please rerun the failing test" text)
        (check default-route-p
               "reply without a route code should use the last active route"))
      (multiple-value-bind (route text default-route-p)
          (xmpp-cli/agent-daemon::resolve-route-reply
           state
           "AaAa focus this panel")
        (check-equal "aaaa" (getf route :code))
        (check-equal "focus this panel" text)
        (check (not default-route-p)
               "reply with an active route code should stay explicit")))))

(deftest agent-reply-unknown-code-does-not-use-default-route
  (with-isolated-data
    (let* ((now (xmpp-cli/util:now-iso8601))
           (route (list :route-id "route-b"
                        :code "bbbb"
                        :identity "route-b"
                        :created-at now
                        :last-seen-at now
                        :last-used-at nil))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :agent-config (list :route-ttl-days 90))))
      (xmpp-cli/agent-routes:save-routes (list route))
      (multiple-value-bind (matched text default-route-p unknown-code)
          (xmpp-cli/agent-daemon::resolve-route-reply
           state
           "ZZZZ please rerun this")
        (check (null matched)
               "unknown route-looking tokens should not match a route")
        (check (null text)
               "unknown route-looking tokens should not be sent as feedback")
        (check (not default-route-p)
               "unknown route-looking tokens should not fall back")
        (check-equal "zzzz" unknown-code))
      (multiple-value-bind (matched text default-route-p unknown-code)
          (xmpp-cli/agent-daemon::resolve-route-reply
           state
           "z9zz please rerun this")
        (check-equal "bbbb" (getf matched :code))
        (check-equal "z9zz please rerun this" text)
        (check default-route-p
               "tokens with digits should still use the default route")
        (check (null unknown-code)
               "tokens with digits should not be treated as route codes")))))

(deftest agent-new-command-resolves-explicit-and-default-route
  (with-isolated-data
    (let* ((older (xmpp-cli/util:now-iso8601 (- (get-universal-time) 60)))
           (newer (xmpp-cli/util:now-iso8601))
           (route-a (list :route-id "route-a"
                          :code "aaaa"
                          :identity "route-a"
                          :created-at older
                          :last-seen-at older
                          :last-used-at nil))
           (route-b (list :route-id "route-b"
                          :code "bbbb"
                          :identity "route-b"
                          :created-at older
                          :last-seen-at newer
                          :last-used-at nil))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :agent-config (list :route-ttl-days 90))))
      (xmpp-cli/agent-routes:save-routes (list route-a route-b))
      (multiple-value-bind (name rest)
          (xmpp-cli/agent-daemon::parse-agent-command "/new AAAA")
        (check-equal "new" name)
        (check-equal "AAAA" rest))
      (check-equal '("AAAA")
                   (xmpp-cli/agent-daemon::split-command-arguments " AAAA "))
      (check-equal "aaaa"
                   (getf (xmpp-cli/agent-daemon::resolve-command-route
                          state
                          "AAAA")
                         :code))
      (check-equal "bbbb"
                   (getf (xmpp-cli/agent-daemon::resolve-command-route
                          state
                          nil)
                         :code)))))

(deftest agent-new-command-allocates-route-for-new-window
  (with-isolated-data
    (let* ((source-identity (xmpp-cli/agent-routes:canonical-route-identity
                             :host "hbox"
                             :tmux-socket "/tmp/tmux-1000/default"
                             :tmux-session-id "$1"
                             :tmux-window-id "@3"
                             :tmux-pane-id "%12"
                             :agent :codex
                             :agent-session "source-session"))
           (source-route (xmpp-cli/agent-routes:ensure-route
                          source-identity
                          :agent :codex
                          :agent-session "source-session"
                          :host "hbox"
                          :cwd "/home/larme/codes/cl-projects/xmpp-cli"
                          :display-cwd "~/codes/cl-projects/xmpp-cli"
                          :tmux-socket "/tmp/tmux-1000/default"
                          :tmux-client-name "/dev/pts/45"
                          :tmux-client-tty "/dev/pts/45"
                          :tmux-session-id "$1"
                          :tmux-window-id "@3"
                          :tmux-pane-id "%12"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :agent-config (list :code-length 4
                                       :route-ttl-days 90)))
           (new-context (list :tmux-socket "/tmp/tmux-1000/default"
                              :tmux-session-id "$1"
                              :tmux-window-id "@99"
                              :tmux-pane-id "%99"
                              :tmux-pane-current-path
                              "/home/larme/codes/cl-projects/xmpp-cli"))
           (new-route (xmpp-cli/agent-daemon::ensure-new-codex-route
                       state
                       source-route
                       new-context))
           (new-code (getf new-route :code)))
      (check-equal 4 (length new-code))
      (check (not (string= new-code (getf source-route :code)))
             "/new should allocate a distinct route code for the new window")
      (check-equal "%99" (getf new-route :tmux-pane-id))
      (check-equal "@99" (getf new-route :tmux-window-id))
      (check-equal "/dev/pts/45" (getf new-route :tmux-client-name))
      (check-equal "~/codes/cl-projects/xmpp-cli"
                   (getf new-route :display-cwd))
      (check-equal new-code
                   (getf (xmpp-cli/agent-routes:find-route-by-code new-code)
                         :code))
      (check-equal new-code
                   (getf (xmpp-cli/agent-routes:last-active-route)
                         :code)))))

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

(deftest codex-permission-notification-includes-tool-name
  (with-isolated-data
    (let* ((config (xmpp-cli/agent-config:set-notify-to
                    (xmpp-cli/agent-config:load-agent-config)
                    "friend@example.org"))
           (payload (xmpp-cli/json:parse-json
                     "{\"hook_event_name\":\"PermissionRequest\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"cwd\":\"/tmp\",\"tool_name\":\"shell_command\",\"permission_mode\":\"default\",\"tool_input\":{\"description\":\"Run tests\",\"command\":\"make test\"}}"))
           (notification
             (xmpp-cli/agent-codex:build-codex-notification
              payload
              config
              :tmux-context nil
              :host "hbox"))
           (body (xmpp-cli/agent-codex:notification-body notification)))
      (check (search "tool=shell_command" body)
             "permission notification header should include the tool name")
      (check (search "permission: default" body)
             "permission notification should include permission mode")
      (check (search "make test" body)
             "permission notification should include tool input detail"))))

(deftest codex-notification-long-parts-repeat-route-metadata
  (with-isolated-data
    (let* ((config (xmpp-cli/agent-config:set-notify-to
                    (xmpp-cli/agent-config:load-agent-config)
                    "friend@example.org"))
           (detail (make-string 5000 :initial-element #\Z))
           (payload (xmpp-cli/json:parse-json
                     (format nil
                             "{\"hook_event_name\":\"Stop\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"session_id\":\"session-1\",\"cwd\":\"/tmp\",\"last_assistant_message\":\"~a\"}"
                             detail)))
           (context (list :tmux-socket "/tmp/tmux-1000/default"
                          :tmux-client-name "/dev/pts/45"
                          :tmux-client-tty "/dev/pts/45"
                          :tmux-session-id "$1"
                          :tmux-window-id "@3"
                          :tmux-pane-id "%12"))
           (notification
             (xmpp-cli/agent-codex:build-codex-notification
              payload
              config
              :tmux-context context
              :host "hbox"))
           (route (xmpp-cli/agent-codex:notification-route notification))
           (code (getf route :code))
           (bodies (xmpp-cli/agent-codex:notification-bodies notification)))
      (check (< 1 (length bodies))
             "long routed notifications should be split")
      (check (every (lambda (body)
                      (and (search (format nil "~a hbox " code) body)
                           (search "codex finished" body)
                           (search "model: gpt-test" body)
                           (search "turn: turn-1" body)
                           (search "cwd: /tmp" body)
                           (<= (length body) 1800)))
                    bodies)
             "every split notification part should repeat route and metadata")
      (check-equal 5000
                   (loop for body in bodies
                         sum (count #\Z body))))))

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

(deftest cli-agent-notify-codex-sends-long-message-in-parts
  (with-isolated-data
    (save-default-test-profile)
    (xmpp-cli/agent-config:save-agent-config
     (xmpp-cli/agent-config:set-notify-to
      (xmpp-cli/agent-config:load-agent-config)
      "friend@example.org"))
    (let* ((detail (make-string 5000 :initial-element #\Z))
           (payload (format nil
                            "{\"hook_event_name\":\"Stop\",\"model\":\"gpt-test\",\"turn_id\":\"turn-1\",\"cwd\":\"/tmp\",\"last_assistant_message\":\"~a\"}"
                            detail)))
      (multiple-value-bind (code events output error-output)
          (run-cli '("agent" "notify-codex") :input payload)
        (declare (ignore output error-output))
        (check-equal 0 code)
        (check (< 1 (length events))
               "long Codex messages should be sent as multiple XMPP messages")
        (check (every (lambda (event)
                        (destructuring-bind (kind profile to body) event
                          (declare (ignore profile))
                          (and (eq kind :send-text)
                               (string= to "friend@example.org")
                               (search "codex finished" body)
                               (search "model: gpt-test" body)
                               (search "turn: turn-1" body)
                               (search "cwd: /tmp" body)
                               (<= (length body) 1800))))
                      events)
               "each notification part should repeat metadata and stay within the message limit")
        (check-equal 5000
                     (loop for event in events
                           sum (count #\Z (fourth event))))
        (check (notany (lambda (event)
                         (search "..." (fourth event)))
                       events)
               "long Codex messages should not be truncated with ellipses")))))

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
