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
          (check (search "room_public: false" text)
                 "agent config should write room booleans as YAML booleans")
          (check-equal "user@example.org"
                       (getf loaded :notify-to))
          (check-equal nil (getf loaded :room-public))
          (check-equal nil (getf loaded :room-persistent))
          (check-equal t (getf loaded :room-members-only))
          (check-equal "moderators" (getf loaded :room-whois))
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
	      (let ((text (xmpp-cli/util:read-file-as-string
	                   (xmpp-cli/agent-routes:routes-pathname))))
	        (check (not (search "version:" text))
	               "routes should not carry a compatibility version wrapper")
	        (check (not (search "routes:" text))
	               "routes should be stored as a top-level YAML list"))
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
    (let ((temp-a (xmpp-cli/persistence:temporary-sibling-pathname
                   (xmpp-cli/agent-routes:routes-pathname)))
          (temp-b (xmpp-cli/persistence:temporary-sibling-pathname
                   (xmpp-cli/agent-routes:routes-pathname)))
          (token-a "route-lock-a")
          (token-b "route-lock-b")
          (lock-path (xmpp-cli/agent-routes:routes-lock-pathname)))
      (xmpp-cli/agent-config:ensure-agent-directory)
      (check (not (equal (namestring temp-a) (namestring temp-b)))
             "route temp paths should be unique")
      (check (xmpp-cli/file-lock:acquire-file-lock
              lock-path
              token-a
              :timeout-seconds 0
              :stale-seconds xmpp-cli/agent-routes::*routes-lock-stale-seconds*
              :use-mtime-p t
              :label "agent route lock")
             "first route lock acquire should succeed")
      (check-signals-error
        (xmpp-cli/file-lock:acquire-file-lock
         lock-path
         token-b
         :timeout-seconds 0
         :stale-seconds xmpp-cli/agent-routes::*routes-lock-stale-seconds*
         :use-mtime-p t
         :label "agent route lock"))
      (check (not (xmpp-cli/file-lock:release-file-lock
                   lock-path
                   token-b
                   :label "agent/routes.lock"))
             "non-owner should not release route lock")
      (check (probe-file (xmpp-cli/agent-routes:routes-lock-pathname))
             "route lock should remain after non-owner release")
      (check (xmpp-cli/file-lock:release-file-lock
              lock-path
              token-a
              :label "agent/routes.lock")
             "owner should release route lock")
      (check (not (probe-file (xmpp-cli/agent-routes:routes-lock-pathname)))
             "route lock should be removed after owner release"))))

(deftest route-lock-keeps-live-owner-past-stale-age
  (with-isolated-data
    (let ((token-a "route-lock-a")
          (token-b "route-lock-b")
          (lock-path (xmpp-cli/agent-routes:routes-lock-pathname)))
      (xmpp-cli/agent-config:ensure-agent-directory)
      (unwind-protect
           (progn
             (check (xmpp-cli/file-lock:acquire-file-lock
                     lock-path
                     token-a
                     :timeout-seconds 0
                     :stale-seconds xmpp-cli/agent-routes::*routes-lock-stale-seconds*
                     :use-mtime-p t
                     :label "agent route lock")
                    "first route lock acquire should succeed")
             (let ((lock (xmpp-cli/file-lock:load-file-lock
                          lock-path
                          :label "agent/routes.lock")))
               (check (xmpp-cli/util:process-exists-p (getf lock :pid))
                      "route lock owner pid should identify a live process"))
             (let ((xmpp-cli/agent-routes::*routes-lock-stale-seconds* -1))
               (check-signals-error
                 (xmpp-cli/file-lock:acquire-file-lock
                  lock-path
                  token-b
                  :timeout-seconds 0
                  :stale-seconds xmpp-cli/agent-routes::*routes-lock-stale-seconds*
                  :use-mtime-p t
                  :label "agent route lock")))
             (check (xmpp-cli/file-lock:file-lock-owned-p
                     lock-path
                     token-a
                     :label "agent/routes.lock")
                    "age alone should not steal a live owner's route lock"))
        (xmpp-cli/file-lock:release-file-lock
         lock-path
         token-a
         :label "agent/routes.lock")))))

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
        (check-equal nil (getf loaded :allowed-senders))
        (check-equal nil (xmpp-cli/agent-config:allowed-senders loaded))))))

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

(deftest daemon-handle-client-ignores-nil-socket
  (check (null (xmpp-cli/agent-daemon::handle-client nil nil))
         "nil control sockets should be ignored"))

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

(defun test-xml-attribute (name value)
  (make-instance 'xmpp:xml-attribute
                 :name name
                 :value value))

(defun test-xml-element (name &key attributes elements data)
  (make-instance 'xmpp:xml-element
                 :name name
                 :attributes attributes
                 :elements elements
                 :data data))

(deftest backend-classifies-raw-stanzas
  (let* ((body (test-xml-element
                :body
                :elements (list (test-xml-element :\#text :data "hello"))))
         (message (test-xml-element
                   :message
                   :attributes (list (test-xml-attribute :from
                                                         "room@example.org/user")
                                     (test-xml-attribute :to
                                                         "bot@example.org")
                                     (test-xml-attribute :id "m1")
                                     (test-xml-attribute :type "groupchat"))
                   :elements (list body)))
         (stanza (xmpp-cli/backend/cl-xmpp::event-stanza-plist message)))
    (check-equal :groupchat (getf stanza :kind))
    (check-equal "room@example.org/user" (getf stanza :from))
    (check-equal "hello" (getf stanza :body)))
  (let* ((query (test-xml-element
                 :query
                 :attributes (list (test-xml-attribute
                                    :xmlns
                                    "http://jabber.org/protocol/disco#info"))))
         (iq (test-xml-element
              :iq
              :attributes (list (test-xml-attribute :from "example.org")
                                (test-xml-attribute :id "disco-1")
                                (test-xml-attribute :type "result"))
              :elements (list query)))
         (stanza (xmpp-cli/backend/cl-xmpp::event-stanza-plist iq)))
    (check-equal :iq (getf stanza :kind))
    (check-equal "disco-1" (getf stanza :id))
    (check-equal "http://jabber.org/protocol/disco#info"
                 (getf stanza :query-xmlns))
    (check-equal nil (getf stanza :disco-features)))
  (let* ((identity (test-xml-element
                    :identity
                    :attributes (list (test-xml-attribute :category
                                                          "conference")
                                      (test-xml-attribute :type "text"))))
         (feature (test-xml-element
                   :feature
                   :attributes (list (test-xml-attribute
                                      :var
                                      xmpp-cli/agent-muc:+muc-feature+))))
         (query (test-xml-element
                 :query
                 :attributes (list (test-xml-attribute
                                    :xmlns
                                    xmpp-cli/agent-muc:+disco-info-xmlns+))
                 :elements (list identity feature)))
         (iq (test-xml-element
              :iq
              :attributes (list (test-xml-attribute :id "disco-2")
                                (test-xml-attribute :type "result"))
              :elements (list query)))
         (stanza (xmpp-cli/backend/cl-xmpp::event-stanza-plist iq)))
    (check-equal (list xmpp-cli/agent-muc:+muc-feature+)
                 (getf stanza :disco-features))
    (check-equal "conference"
                 (getf (first (getf stanza :disco-identities))
                       :category)))
  (let* ((item (test-xml-element
                :item
                :attributes (list (test-xml-attribute :jid
                                                      "conference.example.org")
                                  (test-xml-attribute :name "Rooms"))))
         (query (test-xml-element
                 :query
                 :attributes (list (test-xml-attribute
                                    :xmlns
                                    xmpp-cli/agent-muc:+disco-items-xmlns+))
                 :elements (list item)))
         (iq (test-xml-element
              :iq
              :attributes (list (test-xml-attribute :id "items-1")
                                (test-xml-attribute :type "result"))
              :elements (list query)))
         (stanza (xmpp-cli/backend/cl-xmpp::event-stanza-plist iq)))
    (check-equal "conference.example.org"
                 (getf (first (getf stanza :disco-items)) :jid)))

  (let* ((muc-item (test-xml-element
                    :item
                    :attributes (list (test-xml-attribute :jid
                                                          "user@example.org/phone")
                                      (test-xml-attribute :affiliation "member")
                                      (test-xml-attribute :role "participant"))))
         (status (test-xml-element
                  :status
                  :attributes (list (test-xml-attribute :code "201"))))
         (x (test-xml-element
             :x
             :attributes (list (test-xml-attribute
                                :xmlns
                                "http://jabber.org/protocol/muc#user"))
             :elements (list muc-item status)))
         (presence (test-xml-element
                    :presence
                    :attributes (list (test-xml-attribute
                                       :from
                                       "room@groups.example.org/larme"))
                    :elements (list x)))
         (stanza (xmpp-cli/backend/cl-xmpp::event-stanza-plist presence)))
    (check-equal :presence (getf stanza :kind))
    (check-equal "room@groups.example.org" (getf stanza :room-jid))
    (check-equal "larme" (getf stanza :room-nick))
    (check-equal "user@example.org/phone" (getf stanza :muc-jid))
    (check-equal '("201") (getf stanza :muc-status-codes))))

(deftest muc-service-selection-prefers-conference-domain
  (let ((candidates
          (list
           (list :jid "rooms.example.org"
                 :identities (list (list :category "conference"
                                         :type "text"))
                 :features (list xmpp-cli/agent-muc:+muc-feature+))
           (list :jid "conference.example.org"
                 :identities (list (list :category "conference"
                                         :type "text"))
                 :features (list xmpp-cli/agent-muc:+muc-feature+)))))
    (multiple-value-bind (selected status considered)
        (xmpp-cli/agent-muc:select-muc-service "example.org" candidates)
      (check-equal :ok status)
      (check-equal "conference.example.org" (getf selected :jid))
      (check-equal 2 (length considered)))))

(deftest muc-service-selection-reports-ambiguous-candidates
  (let ((candidates
          (list
           (list :jid "rooms-a.example.org"
                 :features (list xmpp-cli/agent-muc:+muc-feature+))
           (list :jid "rooms-b.example.org"
                 :features (list xmpp-cli/agent-muc:+muc-feature+)))))
    (multiple-value-bind (selected status considered)
        (xmpp-cli/agent-muc:select-muc-service "example.org" candidates)
      (check (null selected) "ambiguous discovery should not choose a service")
      (check-equal :ambiguous status)
      (check-equal 2 (length considered)))))

(deftest muc-service-discovery-caches-result
  (with-isolated-data
    (let* ((config (xmpp-cli/agent-config:load-agent-config))
           (items-calls 0)
           (info-calls 0)
           (items (list (list :jid "conference.example.org"
                              :name "Rooms"))))
      (labels ((request-items (domain)
                 (incf items-calls)
                 (check-equal "example.org" domain)
                 items)
               (request-info (jid)
                 (incf info-calls)
                 (check-equal "conference.example.org" jid)
                 (list :identities (list (list :category "conference"
                                               :type "text"))
                       :features (list xmpp-cli/agent-muc:+muc-feature+))))
        (let ((result (xmpp-cli/agent-muc:resolve-muc-service
                       "example.org"
                       config
                       #'request-items
                       #'request-info)))
          (check-equal "conference.example.org" (getf result :service-jid))
          (check-equal :discovered (getf result :source))
          (check-equal 1 items-calls)
          (check-equal 1 info-calls))
	        (let ((cached (xmpp-cli/agent-muc:resolve-muc-service
	                       "example.org"
	                       config
	                       (lambda (domain)
	                         (declare (ignore domain))
	                         (error "cache should avoid disco#items"))
	                       #'request-info)))
	          (let ((text (xmpp-cli/util:read-file-as-string
	                       (xmpp-cli/agent-muc:muc-services-pathname))))
	            (check (not (search "version:" text))
	                   "MUC cache should not carry a compatibility version wrapper")
	            (check (not (search "services:" text))
	                   "MUC cache should be stored as a top-level YAML list"))
	          (check-equal "conference.example.org" (getf cached :service-jid))
	          (check-equal :cache (getf cached :source)))))))

(defvar *room-test-state* nil)
(defvar *slow-room-join-count* 0)
(defvar *fake-destroy-room-replies-p* t)
(defvar *slow-room-join-lock*
  (bt:make-lock "xmpp-cli slow room test"))

(defmacro with-fake-focus-pane ((focused-routes) &body body)
  (let ((old-focus (gensym "OLD-FOCUS-")))
    `(let ((,old-focus (symbol-function 'xmpp-cli/tmux:focus-pane))
           (,focused-routes nil))
       (unwind-protect
            (progn
              (setf (symbol-function 'xmpp-cli/tmux:focus-pane)
                    (lambda (route)
                      (push route ,focused-routes)
                      :focused))
              ,@body)
         (setf (symbol-function 'xmpp-cli/tmux:focus-pane)
               ,old-focus)))))

(defun wait-for-test-condition (predicate &key (retries 50) (delay 0.02))
  (loop repeat retries
        when (funcall predicate)
          do (return t)
        do (sleep delay)
        finally (return nil)))

(deftest room-state-round-trips-yaml
  (with-isolated-data
    (let ((room (list :room-jid "xmppcli-abcd-room-test@groups.example.org"
                      :room-nick "xmpp-cli"
                      :route-id "route-1"
                      :route-code "abcd"
                      :service-jid "groups.example.org"
                      :room-name "room test"
                      :invited-jids '("user@example.org")
                      :created-at "2026-05-12T00:00:00+08:00"
	                      :last-activity-at "2026-05-12T00:00:00+08:00"
	                      :state "active")))
	      (xmpp-cli/agent-rooms:save-rooms (list room))
	      (let ((text (xmpp-cli/util:read-file-as-string
	                   (xmpp-cli/agent-rooms:rooms-pathname))))
	        (check (not (search "version:" text))
	               "rooms should not carry a compatibility version wrapper")
	        (check (not (search "rooms:" text))
	               "rooms should be stored as a top-level YAML list"))
	      (let ((loaded (first (xmpp-cli/agent-rooms:load-rooms))))
        (check-equal "xmppcli-abcd-room-test@groups.example.org"
                     (getf loaded :room-jid))
        (check-equal '("user@example.org")
                     (getf loaded :invited-jids))
        (check-equal room
                     (xmpp-cli/agent-rooms:find-active-room-by-jid
                      "xmppcli-abcd-room-test@groups.example.org/user"))))))

(deftest room-close-lookup-and-mark-closed
  (with-isolated-data
    (let ((room (list :room-jid "room@groups.example.org"
                      :room-nick "xmpp-cli"
                      :route-id "route-1"
                      :route-code "abcd"
                      :created-at "2026-05-12T00:00:00+08:00"
                      :last-activity-at "2026-05-12T00:00:00+08:00"
                      :state "active")))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (check-equal "room@groups.example.org"
                   (getf (xmpp-cli/agent-rooms:find-active-room-by-route-code
                          "ABCD")
                         :room-jid))
      (xmpp-cli/agent-rooms:mark-room-closed room
                                             "2026-05-12T00:05:00+08:00")
      (check (null (xmpp-cli/agent-rooms:find-active-room-by-route-code
                    "abcd"))
             "closed rooms should not match /room-close lookup")
      (check-equal '("room@groups.example.org route=abcd state=closed last=2026-05-12T00:00:00+08:00")
                   (xmpp-cli/agent-rooms:room-summary-lines
                    (xmpp-cli/agent-rooms:load-rooms))))))

(deftest room-close-marks-local-room-closed-on-destroy-timeout
  (with-isolated-data
    (let* ((room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at "2026-05-12T00:00:00+08:00"
                       :last-activity-at "2026-05-12T00:00:00+08:00"
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :room-nick "xmpp-cli"))))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (let ((*fake-events* nil)
            (*room-test-state* state)
            (*fake-destroy-room-replies-p* nil)
            (xmpp-cli/agent-daemon::*default-iq-timeout-seconds* 0))
        (multiple-value-bind (destroyed destroy-error)
            (xmpp-cli/agent-daemon::close-room
             state
             room
             :reason "test close")
          (check (null destroyed)
                 "server destroy should be reported as not acknowledged")
          (check destroy-error
                 "destroy timeout should be returned to the caller")
          (check-equal "closed"
                       (getf (first (xmpp-cli/agent-rooms:load-rooms))
                             :state))
          (check (some (lambda (event)
                         (eq (first event) :destroy-room))
                       *fake-events*)
                 "close should still try the owner destroy IQ")
          (check (some (lambda (event)
                         (eq (first event) :leave-room))
                       *fake-events*)
                 "close should leave the MUC even if destroy is not acknowledged")
          (let ((log (xmpp-cli/util:read-file-as-string
                      (xmpp-cli/agent-rooms:room-log-pathname room))))
            (check (search "room closed: test close" log)
                   "close should finalize a room log")))))))

(deftest room-local-close-command-closes-current-room
  (with-isolated-data
    (let* ((room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at "2026-05-12T00:00:00+08:00"
                       :last-activity-at "2026-05-12T00:00:00+08:00"
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :room-nick "xmpp-cli"
                                       :allowed-senders
                                       '("friend@example.org")))))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (xmpp-cli/agent-daemon::remember-room-occupant
       state
       (list :kind :presence
             :from "room@groups.example.org/friend"
             :room-jid "room@groups.example.org"
             :room-nick "friend"
             :muc-user-p t
             :muc-jid "friend@example.org/phone"))
      (let ((*fake-events* nil)
            (*room-test-state* state)
            (*fake-destroy-room-replies-p* t))
        (xmpp-cli/agent-daemon::handle-room-message
         state
         (list :kind :groupchat
               :from "room@groups.example.org/friend"
               :room-jid "room@groups.example.org"
               :room-nick "friend"
               :body "/close"))
        (check-equal "closed"
                     (getf (first (xmpp-cli/agent-rooms:load-rooms))
                           :state))
        (check (some (lambda (event)
                       (eq (first event) :send-room-message))
                     *fake-events*)
               "room /close should acknowledge in the room before closing")
        (check (some (lambda (event)
                       (eq (first event) :destroy-room))
                     *fake-events*)
               "room /close should try to destroy the current room")
        (check (some (lambda (event)
                       (eq (first event) :leave-room))
                     *fake-events*)
               "room /close should leave the current room")
        (let ((log (xmpp-cli/util:read-file-as-string
                    (xmpp-cli/agent-rooms:room-log-pathname room))))
          (check (search "/close" log)
                 "room log should include the user close command")
          (check (search "room closed: closed by xmpp-cli room user" log)
                 "room log should include the close event"))))))

(deftest direct-room-close-command-closes-room-by-route-code
  (with-isolated-data
    (let* ((room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at "2026-05-12T00:00:00+08:00"
                       :last-activity-at "2026-05-12T00:00:00+08:00"
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :room-nick "xmpp-cli"))))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (let ((*fake-events* nil)
            (*room-test-state* state)
            (*fake-destroy-room-replies-p* t))
        (xmpp-cli/agent-daemon::handle-direct-command
         state
         "friend@example.org"
         "/room-close ABCD")
        (check-equal "closed"
                     (getf (first (xmpp-cli/agent-rooms:load-rooms))
                           :state))
        (check (some (lambda (event)
                       (eq (first event) :destroy-room))
                     *fake-events*)
               "direct /room-close should try to destroy the room")
        (check (some (lambda (event)
                       (and (eq (first event) :send-connected-text)
                            (search "closed room" (third event))))
                     *fake-events*)
               "direct /room-close should acknowledge in direct chat")
        (check (some (lambda (event)
                       (and (eq (first event) :send-connected-text)
                            (search "log:" (third event))))
                     *fake-events*)
               "direct /room-close should include the log path")))))

(deftest room-prefixed-direct-command-is-not-routed-inside-room
  (with-isolated-data
    (let* ((room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at "2026-05-12T00:00:00+08:00"
                       :last-activity-at "2026-05-12T00:00:00+08:00"
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :room-nick "xmpp-cli"
                                       :allowed-senders
                                       '("friend@example.org")))))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (xmpp-cli/agent-daemon::remember-room-occupant
       state
       (list :kind :presence
             :from "room@groups.example.org/friend"
             :room-jid "room@groups.example.org"
             :room-nick "friend"
             :muc-user-p t
             :muc-jid "friend@example.org/phone"))
      (let ((*fake-events* nil))
        (xmpp-cli/agent-daemon::handle-room-message
         state
         (list :kind :groupchat
               :from "room@groups.example.org/friend"
               :room-jid "room@groups.example.org"
               :room-nick "friend"
               :body "/room-close"))
        (check (some (lambda (event)
                       (and (eq (first event) :send-room-message)
                            (search "use /close" (third event))))
                     *fake-events*)
               "room-local commands should suggest the unprefixed form")
        (check (not (some (lambda (event)
                            (eq (first event) :destroy-room))
                          *fake-events*))
               "prefixed direct command should not close a room from inside the room")))))

(deftest room-local-focus-command-focuses-bound-route
  (with-isolated-data
    (let* ((now (xmpp-cli/util:now-iso8601))
           (route (list :route-id "route-1"
                        :code "abcd"
                        :identity "route-1"
                        :created-at now
                        :last-seen-at now
                        :last-used-at nil
                        :last-direct-used-at nil))
           (room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at now
                       :last-activity-at now
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :room-nick "xmpp-cli"
                                       :route-ttl-days 90
                                       :allowed-senders
                                       '("friend@example.org")))))
      (xmpp-cli/agent-routes:save-routes (list route))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (xmpp-cli/agent-daemon::remember-room-occupant
       state
       (list :kind :presence
             :from "room@groups.example.org/friend"
             :room-jid "room@groups.example.org"
             :room-nick "friend"
             :muc-user-p t
             :muc-jid "friend@example.org/phone"))
      (with-fake-focus-pane (focused-routes)
        (let ((*fake-events* nil))
          (xmpp-cli/agent-daemon::handle-room-message
           state
           (list :kind :groupchat
                 :from "room@groups.example.org/friend"
                 :room-jid "room@groups.example.org"
                 :room-nick "friend"
                 :body "/focus"))
          (check-equal "abcd" (getf (first focused-routes) :code))
          (check (some (lambda (event)
                         (and (eq (first event) :send-room-message)
                              (search "focused abcd" (third event))))
                       *fake-events*)
                 "room /focus should acknowledge in the room")
          (let ((updated (xmpp-cli/agent-routes:find-route-by-code "abcd")))
            (check (getf updated :last-used-at)
                   "room /focus should update route activity")
            (check (null (getf updated :last-direct-used-at))
                   "room /focus should not update direct-chat activity")))))))

(deftest room-state-keeps-one-active-room-per-route
  (with-isolated-data
    (let ((old-room (list :room-jid "old@groups.example.org"
                          :route-id "route-1"
                          :route-code "abcd"
                          :created-at "2026-05-12T00:00:00+08:00"
                          :state "active"))
          (new-room (list :room-jid "new@groups.example.org"
                          :route-id "route-1"
                          :route-code "abcd"
                          :created-at "2026-05-12T00:01:00+08:00"
                          :state "active")))
      (xmpp-cli/agent-rooms:save-rooms (list old-room))
      (xmpp-cli/agent-rooms:upsert-room new-room)
      (let ((rooms (xmpp-cli/agent-rooms:active-rooms)))
        (check-equal 1 (length rooms))
        (check-equal "new@groups.example.org"
                     (getf (first rooms) :room-jid))))))

(deftest room-slug-and-occupant-parsing
  (check-equal "parser-fix"
               (xmpp-cli/agent-rooms:sanitize-room-slug "Parser fix!"))
  (check-equal "room"
               (xmpp-cli/agent-rooms:sanitize-room-slug "!!!"))
  (multiple-value-bind (room nick)
      (xmpp-cli/agent-rooms:parse-room-occupant-jid
       "xmppcli-abcd-room@groups.example.org/larme")
    (check-equal "xmppcli-abcd-room@groups.example.org" room)
    (check-equal "larme" nick)))

(deftest room-command-requires-explicit-route
  (multiple-value-bind (route-code room-name)
      (xmpp-cli/agent-daemon::parse-room-command-arguments
       "ABCD parser fix")
    (check-equal "abcd" route-code)
    (check-equal "parser fix" room-name))
  (multiple-value-bind (route-code room-name)
      (xmpp-cli/agent-daemon::parse-room-command-arguments "")
    (check (null route-code) "empty /room should not produce a route")
    (check (null room-name) "empty /room should not produce a room name")))

(deftest room-config-fields-respect-returned-form
  (let* ((requested (xmpp-cli/agent-daemon::requested-room-config-fields
                    (list :room-public nil
                          :room-persistent nil
                          :room-members-only t
                          :room-whois "moderators")
                    "Parser Fix"))
         (selected (xmpp-cli/agent-daemon::select-room-config-fields
                    (list (list :var "FORM_TYPE")
                          (list :var "muc#roomconfig_roomname")
                          (list :var "muc#roomconfig_membersonly"))
                    requested)))
    (check-equal '("FORM_TYPE"
                   "muc#roomconfig_roomname"
                   "muc#roomconfig_membersonly")
                 (mapcar #'first selected))))

(defclass slow-room-backend (fake-backend) ())

(defmethod xmpp-cli/backend:join-room ((backend fake-backend)
                                       connection
                                       room-full-jid)
  (declare (ignore backend connection))
  (push (list :join-room room-full-jid) *fake-events*)
  (xmpp-cli/agent-daemon::resolve-pending-room-join
   *room-test-state*
   (list :kind :presence
         :from room-full-jid
         :muc-status-codes '("201"))))

(defmethod xmpp-cli/backend:join-room ((backend slow-room-backend)
                                       connection
                                       room-full-jid)
  (declare (ignore backend connection))
  (bt:with-lock-held (*slow-room-join-lock*)
    (incf *slow-room-join-count*))
  (sleep 0.2)
  (xmpp-cli/agent-daemon::resolve-pending-room-join
   *room-test-state*
   (list :kind :presence
         :from room-full-jid
         :muc-status-codes '("201"))))

(defmethod xmpp-cli/backend:request-room-config ((backend fake-backend)
                                                 connection
                                                 room-jid
                                                 id)
  (declare (ignore backend connection))
  (push (list :request-room-config room-jid id) *fake-events*)
  (xmpp-cli/agent-daemon::resolve-pending-iq
   *room-test-state*
   (list :kind :iq
         :id id
         :type "result"
         :data-form-fields (list (list :var "FORM_TYPE")
                                 (list :var "muc#roomconfig_roomname")
                                 (list :var "muc#roomconfig_membersonly")))))

(defmethod xmpp-cli/backend:submit-room-config ((backend fake-backend)
                                                connection
                                                room-jid
                                                id
                                                fields)
  (declare (ignore backend connection))
  (push (list :submit-room-config room-jid fields) *fake-events*)
  (xmpp-cli/agent-daemon::resolve-pending-iq
   *room-test-state*
   (list :kind :iq :id id :type "result")))

(defmethod xmpp-cli/backend:grant-room-membership ((backend fake-backend)
                                                   connection
                                                   room-jid
                                                   id
                                                   jid)
  (declare (ignore backend connection))
  (push (list :grant-room-membership room-jid jid) *fake-events*)
  (xmpp-cli/agent-daemon::resolve-pending-iq
   *room-test-state*
   (list :kind :iq :id id :type "result")))

(defmethod xmpp-cli/backend:send-direct-room-invite ((backend fake-backend)
                                                    connection
                                                    to
                                                    room-jid
                                                    reason)
  (declare (ignore backend connection))
  (push (list :send-direct-room-invite to room-jid reason) *fake-events*)
  :sent)

(defmethod xmpp-cli/backend:send-connected-text ((backend fake-backend)
                                                connection
                                                to
                                                body)
  (declare (ignore backend connection))
  (push (list :send-connected-text to body) *fake-events*)
  :sent)

(defmethod xmpp-cli/backend:send-room-message ((backend fake-backend)
                                               connection
                                               room-jid
                                               body)
  (declare (ignore backend connection))
  (push (list :send-room-message room-jid body) *fake-events*)
  :sent)

(defmethod xmpp-cli/backend:destroy-room ((backend fake-backend)
                                          connection
                                          room-jid
                                          id
                                          &key reason)
  (declare (ignore backend connection))
  (push (list :destroy-room room-jid id reason) *fake-events*)
  (when *fake-destroy-room-replies-p*
    (xmpp-cli/agent-daemon::resolve-pending-iq
     *room-test-state*
     (list :kind :iq :id id :type "result"))))

(defmethod xmpp-cli/backend:leave-room ((backend fake-backend)
                                        connection
                                        room-full-jid)
  (declare (ignore backend connection))
  (push (list :leave-room room-full-jid) *fake-events*)
  :sent)

(deftest room-rejoin-repairs-recreated-room
  (let* ((state (xmpp-cli/agent-daemon::make-daemon-state
                 :backend (make-instance 'fake-backend)
                 :connection :fake-connection
                 :xmpp-status :connected
                 :agent-config (list :room-nick "xmpp-cli"
                                     :room-public nil
                                     :room-persistent nil
                                     :room-members-only t
                                     :room-whois "moderators")))
         (room (list :room-jid "room@groups.example.org"
                     :room-nick "xmpp-cli"
                     :route-code "abcd"
                     :room-name "room test"
                     :invited-jids '("user@example.org"
                                     "notify@example.org"))))
    (let ((*room-test-state* state)
          (*fake-events* nil))
      (xmpp-cli/agent-daemon::rejoin-room state room)
      (let ((events (reverse *fake-events*)))
        (check-equal :join-room (caar events))
        (check-equal '("user@example.org" "notify@example.org")
                     (mapcar #'third
                             (remove-if-not
                              (lambda (event)
                                (eq (first event) :grant-room-membership))
                              events)))
        (check-equal '("user@example.org" "notify@example.org")
                     (mapcar #'second
                             (remove-if-not
                              (lambda (event)
                                (eq (first event) :send-direct-room-invite))
                              events)))))))

(deftest concurrent-room-commands-create-one-room
  (let ((old-data-directory xmpp-cli/util::*data-directory*)
        (old-room-test-state *room-test-state*)
        (old-join-count *slow-room-join-count*)
        (thread-a nil)
        (thread-b nil))
    (unwind-protect
         (let* ((data-directory (make-test-directory))
                (state nil)
                (route (list :route-id "route-1"
                             :code "abcd"))
                (result-a nil)
                (result-b nil)
                (error-a nil)
                (error-b nil))
           (setf xmpp-cli/util::*data-directory* data-directory)
           (xmpp-cli/agent-muc:cache-muc-service "example.org"
                                                 "groups.example.org")
           (setf state
                 (xmpp-cli/agent-daemon::make-daemon-state
                  :backend (make-instance 'slow-room-backend)
                  :connection :fake-connection
                  :xmpp-status :connected
                  :profile (list :jid "bot@example.org"
                                 :domain "example.org")
                  :agent-config (list :notify-to "user@example.org"
                                      :room-nick "xmpp-cli"
                                      :room-public nil
                                      :room-persistent nil
                                      :room-members-only t
                                      :room-whois "moderators")))
           (flet ((create-room ()
                    (multiple-value-list
                     (xmpp-cli/agent-daemon::create-room-binding
                      state
                      "user@example.org/phone"
                      route
                      "race"))))
             (setf *room-test-state* state)
             (setf *slow-room-join-count* 0)
             (setf thread-a
                   (bt:make-thread
                    (lambda ()
                      (handler-case
                          (setf result-a (create-room))
                        (error (condition)
                          (setf error-a condition))))
                    :name "xmpp-cli test room race a"))
             (sleep 0.05)
             (setf thread-b
                   (bt:make-thread
                    (lambda ()
                      (handler-case
                          (setf result-b (create-room))
                        (error (condition)
                          (setf error-b condition))))
                    :name "xmpp-cli test room race b"))
             (loop repeat 50
                   while (or (and thread-a (bt:thread-alive-p thread-a))
                             (and thread-b (bt:thread-alive-p thread-b)))
                   do (sleep 0.1))
             (check (not (and thread-a (bt:thread-alive-p thread-a)))
                    "first room worker should finish")
             (check (not (and thread-b (bt:thread-alive-p thread-b)))
                    "second room worker should finish")
             (check (null error-a)
                    "first room worker failed: ~a"
                    error-a)
             (check (null error-b)
                    "second room worker failed: ~a"
                    error-b)
             (check-equal 1 *slow-room-join-count*)
             (check-equal 1 (length (xmpp-cli/agent-rooms:active-rooms)))
             (check-equal 1
                          (count t
                                 (list (second result-a)
                                       (second result-b))))))
      (when (and thread-a (bt:thread-alive-p thread-a))
        (ignore-errors
          (bt:destroy-thread thread-a)))
      (when (and thread-b (bt:thread-alive-p thread-b))
        (ignore-errors
          (bt:destroy-thread thread-b)))
      (setf xmpp-cli/util::*data-directory* old-data-directory)
      (setf *room-test-state* old-room-test-state)
      (setf *slow-room-join-count* old-join-count))))

(deftest daemon-notify-routes-to-bound-room
  (with-isolated-data
    (let* ((profile (test-profile))
           (digest (xmpp-cli/agent-ipc:profile-digest profile))
           (room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at "2026-05-12T00:00:00+08:00"
                       :last-activity-at "2026-05-12T00:00:00+08:00"
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :profile (list :jid "bot@example.org")
                   :token "token"
                   :control (list :profile-digest digest)
                   :agent-config (list :notify-to "friend@example.org"
                                       :allowed-senders
                                       '("friend@example.org")))))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (xmpp-cli/agent-daemon::remember-room-occupant
       state
       (list :kind :presence
             :from "room@groups.example.org/friend"
             :room-jid "room@groups.example.org"
             :room-nick "friend"
             :muc-user-p t
             :muc-jid "friend@example.org/phone"))
      (let* ((*fake-events* nil)
             (response
               (xmpp-cli/agent-daemon::handle-control-request
                state
                (list :token "token"
                      :op :notify
                      :route-id "route-1"
                      :fallback-to "friend@example.org"
                      :body "done"
                      :expected-profile-digest digest))))
        (check (getf response :ok)
               "route notification should succeed")
        (check-equal :room (getf response :target-kind))
        (check-equal "room@groups.example.org" (getf response :target))
        (check-equal '(:send-room-message "room@groups.example.org" "done")
                     (first *fake-events*))))))

(deftest daemon-notify-bound-room-falls-back-when-no-allowed-occupant-known
  (with-isolated-data
    (let* ((profile (test-profile))
           (digest (xmpp-cli/agent-ipc:profile-digest profile))
           (room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at "2026-05-12T00:00:00+08:00"
                       :last-activity-at "2026-05-12T00:00:00+08:00"
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :profile (list :jid "bot@example.org")
                   :token "token"
                   :control (list :profile-digest digest)
                   :agent-config (list :notify-to "friend@example.org"
                                       :allowed-senders
                                       '("friend@example.org")))))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (let* ((*fake-events* nil)
             (response
               (xmpp-cli/agent-daemon::handle-control-request
                state
                (list :token "token"
                      :op :notify
                      :route-id "route-1"
                      :fallback-to "friend@example.org"
                      :body "done"
                      :expected-profile-digest digest))))
        (check (getf response :ok)
               "route notification should fall back to direct chat")
        (check-equal :jid (getf response :target-kind))
        (check-equal "friend@example.org" (getf response :target))
        (check-equal "room@groups.example.org" (getf response :room))
        (check-equal '(:send-connected-text "friend@example.org" "done")
                     (first *fake-events*))))))

(deftest daemon-notify-falls-back-to-direct-when-route-has-no-room
  (with-isolated-data
    (let* ((profile (test-profile))
           (digest (xmpp-cli/agent-ipc:profile-digest profile))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :token "token"
                   :control (list :profile-digest digest)
                   :agent-config (list :notify-to "friend@example.org"))))
      (let* ((*fake-events* nil)
             (response
               (xmpp-cli/agent-daemon::handle-control-request
                state
                (list :token "token"
                      :op :notify
                      :route-id "route-1"
                      :fallback-to "friend@example.org"
                      :body "done"
                      :expected-profile-digest digest))))
        (check (getf response :ok)
               "route notification without room should use direct fallback")
        (check-equal :jid (getf response :target-kind))
        (check-equal "friend@example.org" (getf response :target))
        (check-equal '(:send-connected-text "friend@example.org" "done")
                     (first *fake-events*))))))

(deftest codex-hook-installer-permission-timeout-is-not-approval-wait
  (let* ((script (xmpp-cli/util:read-file-as-string
                  (asdf:system-relative-pathname
                   "xmpp-cli"
                   "scripts/install-codex-xmpp-hook.sh")))
         (permission-section (search "[[hooks.PermissionRequest.hooks]]"
                                     script))
         (timeout (and permission-section
                       (search "timeout = 20"
                               script
                               :start2 permission-section))))
    (check timeout
           "PermissionRequest hook should use normal notification timeout")))

(deftest room-missing-route-stays-open-and-replies-in-room
  (with-isolated-data
    (let* ((room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "missing-route"
                       :route-code "abcd"
                       :created-at "2026-05-12T00:00:00+08:00"
                       :last-activity-at "2026-05-12T00:00:00+08:00"
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :room-nick "xmpp-cli"
                                       :route-ttl-days 90
                                       :allowed-senders
                                       '("friend@example.org")))))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (xmpp-cli/agent-daemon::remember-room-occupant
       state
       (list :kind :presence
             :from "room@groups.example.org/friend"
             :room-jid "room@groups.example.org"
             :room-nick "friend"
             :muc-user-p t
             :muc-jid "friend@example.org/phone"))
      (let ((*fake-events* nil))
        (xmpp-cli/agent-daemon::handle-room-message
         state
         (list :kind :groupchat
               :from "room@groups.example.org/friend"
               :room-jid "room@groups.example.org"
               :room-nick "friend"
               :body "hello"))
        (check-equal "active"
                     (getf (first (xmpp-cli/agent-rooms:load-rooms))
                           :state))
        (check (some (lambda (event)
                       (and (eq (first event) :send-room-message)
                            (search "route abcd is no longer active"
                                    (third event))))
                     *fake-events*)
               "room should get a clear inactive route reply")
        (check (not (some (lambda (event)
                            (eq (first event) :destroy-room))
                          *fake-events*))
               "missing routes should not auto-destroy rooms")))))

(deftest room-missing-pane-stays-open-and-replies-in-room
  (with-isolated-data
    (let* ((now (xmpp-cli/util:now-iso8601))
           (route (list :route-id "route-1"
                        :code "abcd"
                        :identity "route-1"
                        :created-at now
                        :last-seen-at now
                        :last-used-at nil
                        :last-direct-used-at nil
                        :tmux-pane-id "%1"))
           (room (list :room-jid "room@groups.example.org"
                       :room-nick "xmpp-cli"
                       :route-id "route-1"
                       :route-code "abcd"
                       :created-at now
                       :last-activity-at now
                       :state "active"))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :room-nick "xmpp-cli"
                                       :route-ttl-days 90
                                       :allowed-senders
                                       '("friend@example.org"))))
           (old-pane-exists (symbol-function 'xmpp-cli/tmux:pane-exists-p)))
      (xmpp-cli/agent-routes:save-routes (list route))
      (xmpp-cli/agent-rooms:save-rooms (list room))
      (xmpp-cli/agent-daemon::remember-room-occupant
       state
       (list :kind :presence
             :from "room@groups.example.org/friend"
             :room-jid "room@groups.example.org"
             :room-nick "friend"
             :muc-user-p t
             :muc-jid "friend@example.org/phone"))
      (unwind-protect
           (let ((*fake-events* nil))
             (setf (symbol-function 'xmpp-cli/tmux:pane-exists-p)
                   (lambda (route)
                     (declare (ignore route))
                     nil))
             (xmpp-cli/agent-daemon::handle-room-message
              state
              (list :kind :groupchat
                    :from "room@groups.example.org/friend"
                    :room-jid "room@groups.example.org"
                    :room-nick "friend"
                    :body "hello"))
             (check-equal "active"
                          (getf (first (xmpp-cli/agent-rooms:load-rooms))
                                :state))
             (check (some (lambda (event)
                            (and (eq (first event) :send-room-message)
                                 (search "route abcd is no longer active"
                                         (third event))))
                          *fake-events*)
                    "room should get a clear inactive pane reply")
             (check (not (some (lambda (event)
                                 (eq (first event) :destroy-room))
                               *fake-events*))
                    "missing panes should not auto-destroy rooms"))
        (setf (symbol-function 'xmpp-cli/tmux:pane-exists-p)
              old-pane-exists)))))

(deftest room-occupant-authorization-uses-real-jid
  (let* ((state (xmpp-cli/agent-daemon::make-daemon-state
                 :agent-config (list :allowed-senders
                                     '("user@example.org"))))
         (room (list :room-jid "room@groups.example.org"
                     :room-nick "xmpp-cli"))
         (presence (list :kind :presence
                         :from "room@groups.example.org/larme"
                         :room-jid "room@groups.example.org"
                         :room-nick "larme"
                         :muc-user-p t
                         :muc-jid "user@example.org/phone"
                         :muc-affiliation "member"
                         :muc-role "participant"))
         (message (list :kind :groupchat
                        :from "room@groups.example.org/larme"
                        :room-jid "room@groups.example.org"
                        :room-nick "larme"
                        :body "please test")))
    (xmpp-cli/agent-daemon::remember-room-occupant state presence)
    (check-equal "user@example.org"
                 (xmpp-cli/agent-daemon::authorized-room-sender-p
                  state
                  room
                  message))))

(deftest room-occupants-clear-on-disconnect
  (let* ((state (xmpp-cli/agent-daemon::make-daemon-state))
         (presence (list :kind :presence
                         :from "room@groups.example.org/larme"
                         :room-jid "room@groups.example.org"
                         :room-nick "larme"
                         :muc-user-p t
                         :muc-jid "user@example.org/phone")))
    (xmpp-cli/agent-daemon::remember-room-occupant state presence)
    (check (xmpp-cli/agent-daemon::room-occupant
            state
            "room@groups.example.org"
            "larme")
           "occupant should be cached before disconnect")
    (xmpp-cli/agent-daemon::mark-disconnected state "closed")
    (check (null (xmpp-cli/agent-daemon::room-occupant
                  state
                  "room@groups.example.org"
                  "larme"))
           "occupants should not survive reconnect boundaries")))

(deftest room-route-activity-does-not-change-direct-default
  (with-isolated-data
    (let* ((older (xmpp-cli/util:now-iso8601 (- (get-universal-time) 60)))
           (newer (xmpp-cli/util:now-iso8601))
           (route-a (list :route-id "route-a"
                          :code "aaaa"
                          :identity "route-a"
                          :created-at older
                          :last-seen-at older
                          :last-used-at nil
                          :last-direct-used-at nil))
           (route-b (list :route-id "route-b"
                          :code "bbbb"
                          :identity "route-b"
                          :created-at older
                          :last-seen-at newer
                          :last-used-at nil
                          :last-direct-used-at nil)))
      (xmpp-cli/agent-routes:save-routes (list route-a route-b))
      (xmpp-cli/agent-routes:mark-route-used
       route-a
       :now (xmpp-cli/util:now-iso8601 (+ (get-universal-time) 60))
       :direct-p nil)
      (check-equal "bbbb"
                   (getf (xmpp-cli/agent-routes:last-active-route)
                         :code))
      (xmpp-cli/agent-routes:mark-route-used
       route-a
       :now (xmpp-cli/util:now-iso8601 (+ (get-universal-time) 120))
       :direct-p t)
      (check-equal "aaaa"
                   (getf (xmpp-cli/agent-routes:last-active-route)
                         :code)))))

(deftest pending-iq-resolves-from-incoming-stanza
  (let ((state (xmpp-cli/agent-daemon::make-daemon-state
                :xmpp-status :connected
                :connection :fake-connection))
        (seen-id nil)
        (thread nil))
    (unwind-protect
         (progn
           (setf thread
                 (bt:make-thread
                  (lambda ()
                    (loop until seen-id do (sleep 0.01))
                    (sleep 0.05)
                    (xmpp-cli/agent-daemon::handle-incoming-stanza
                     state
                     (list :kind :iq
                           :id seen-id
                           :type "result"
                           :from "example.org")))
                  :name "xmpp-cli test IQ resolver"))
           (let ((response
                   (xmpp-cli/agent-daemon::send-iq-and-wait
                    state
                    (lambda (connection id)
                      (check-equal :fake-connection connection)
                      (setf seen-id id))
                    :timeout-seconds 2)))
             (check-equal seen-id (getf response :id))
             (check-equal 0
                          (hash-table-count
                           (xmpp-cli/agent-daemon::daemon-state-pending-iqs
                            state)))))
      (when (and thread (bt:thread-alive-p thread))
        (ignore-errors
          (bt:destroy-thread thread))))))

(deftest pending-iq-timeout-removes-pending-entry
  (let ((state (xmpp-cli/agent-daemon::make-daemon-state
                :xmpp-status :connected
                :connection :fake-connection)))
    (check-signals-error
      (xmpp-cli/agent-daemon::send-iq-and-wait
       state
       (lambda (connection id)
         (declare (ignore connection id)))
       :timeout-seconds 0.05))
    (check-equal 0
                 (hash-table-count
                  (xmpp-cli/agent-daemon::daemon-state-pending-iqs state)))))

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

(deftest tmux-paste-text-does-not-focus-pane
  (with-isolated-data
    (let ((old-focus (symbol-function 'xmpp-cli/tmux:focus-pane))
          (old-run-tmux (symbol-function 'xmpp-cli/tmux::run-tmux))
          (focus-count 0)
          (commands nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'xmpp-cli/tmux:focus-pane)
                   (lambda (route)
                     (declare (ignore route))
                     (incf focus-count)))
             (setf (symbol-function 'xmpp-cli/tmux::run-tmux)
                   (lambda (arguments &key socket)
                     (push (list arguments socket) commands)
                     ""))
             (xmpp-cli/tmux:paste-text-and-enter
              (list :code "abcd"
                    :tmux-socket "/tmp/tmux-1000/default"
                    :tmux-pane-id "%12")
              "please run tests")
             (check-equal 0 focus-count)
             (check-equal 3 (length commands)))
        (setf (symbol-function 'xmpp-cli/tmux:focus-pane) old-focus)
        (setf (symbol-function 'xmpp-cli/tmux::run-tmux) old-run-tmux)))))

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

(deftest agent-focus-command-resolves-explicit-and-default-route
  (with-isolated-data
    (let* ((older (xmpp-cli/util:now-iso8601 (- (get-universal-time) 60)))
           (newer (xmpp-cli/util:now-iso8601))
           (route-a (list :route-id "route-a"
                          :code "aaaa"
                          :identity "route-a"
                          :created-at older
                          :last-seen-at older
                          :last-used-at nil
                          :last-direct-used-at nil))
           (route-b (list :route-id "route-b"
                          :code "bbbb"
                          :identity "route-b"
                          :created-at older
                          :last-seen-at newer
                          :last-used-at nil
                          :last-direct-used-at nil))
           (state (xmpp-cli/agent-daemon::make-daemon-state
                   :backend (make-instance 'fake-backend)
                   :connection :fake-connection
                   :xmpp-status :connected
                   :agent-config (list :route-ttl-days 90))))
      (xmpp-cli/agent-routes:save-routes (list route-a route-b))
      (with-fake-focus-pane (focused-routes)
        (let ((*fake-events* nil))
          (xmpp-cli/agent-daemon::handle-direct-command
           state
           "friend@example.org"
           "/focus")
          (check-equal "bbbb" (getf (first focused-routes) :code))
          (check (search "focused bbbb (default route)"
                         (third (first *fake-events*)))
                 "direct /focus should use the direct default route")
          (xmpp-cli/agent-daemon::handle-direct-command
           state
           "friend@example.org"
           "/focus AAAA")
          (check-equal "aaaa" (getf (first focused-routes) :code))
          (check (search "focused aaaa"
                         (third (first *fake-events*)))
                 "direct /focus should accept an explicit route code")
          (let ((updated (xmpp-cli/agent-routes:find-route-by-code "aaaa")))
            (check (getf updated :last-direct-used-at)
                   "direct /focus should update direct-chat activity")))))))

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
      (let ((target (xmpp-cli/agent-codex:notification-target notification-a)))
        (check-equal :route (getf target :kind))
        (check-equal (getf route-a :route-id) (getf target :route-id))
        (check-equal code (getf target :route-code))
        (check-equal "friend@example.org" (getf target :fallback-jid)))
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
             "permission notification should include tool input detail")
      (check (not (search "reply options:" body))
             "non-actionable permission notifications should not offer approval options"))))

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
