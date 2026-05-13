(in-package #:xmpp-cli/agent-daemon)

(defvar *room-binding-lock*
  (bt:make-lock "xmpp-cli room binding"))

(defun bare-jid (jid)
  (let ((text (or jid "")))
    (let ((slash (position #\/ text)))
      (if slash
          (subseq text 0 slash)
          text))))

(defun parse-agent-reply (body)
  (let ((trimmed (string-trim *reply-whitespace* (or body ""))))
    (when (plusp (length trimmed))
      (let ((end (position-if (lambda (char)
                                (member char *reply-whitespace* :test #'char=))
                              trimmed)))
        (if end
            (values (string-downcase (subseq trimmed 0 end))
                    (string-trim *reply-whitespace*
                                 (subseq trimmed end)))
            (values (string-downcase trimmed) ""))))))

(defun send-daemon-note (state to text)
  (when (and to (plusp (length to)))
    (multiple-value-bind (ok error-text) (daemon-send-text state to text)
      (declare (ignore ok error-text)))))

(defun send-room-note (state room-jid text)
  (when (and room-jid (plusp (length room-jid)))
    (handler-case
        (let ((backend (daemon-state-backend state))
              (connection (current-xmpp-connection-or-error state)))
          (call-with-xmpp-write-lock
           state
           (lambda ()
             (send-room-message backend connection room-jid text))))
      (error (condition)
        (format *error-output*
                "~&xmpp-cli daemon: could not send room note to ~a: ~a~%"
                room-jid
                condition)
        (finish-output *error-output*)))))

(defun authorized-sender-p (state from)
  (let* ((sender (bare-jid from))
         (allowed (allowed-senders (daemon-state-agent-config state))))
    (and (plusp (length sender))
         (member sender allowed :test #'string-equal))))

(defun apply-route-reply (route text)
  (if (plusp (length text))
      (paste-text-and-enter route text)
      (focus-pane route)))

(defun reply-text (body)
  (string-trim *reply-whitespace* (or body "")))

(defun command-text-p (text)
  (and (plusp (length text))
       (char= (char text 0) #\/)))

(defun route-code-character-p (char)
  (or (and (char>= char #\a)
           (char<= char #\z))
      (and (char>= char #\A)
           (char<= char #\Z))))

(defun route-code-token-p (state token)
  (let ((code-length (getf (daemon-state-agent-config state)
                           :code-length
                           4)))
    (and (stringp token)
         (= (length token) code-length)
         (every #'route-code-character-p token))))

(defun active-routes-for-state (state)
  (load-active-routes :route-ttl-days (route-ttl-days state)))

(defun resolve-route-reply (state body)
  (let ((message (reply-text body)))
    (when (plusp (length message))
      (let* ((routes (active-routes-for-state state))
             (ttl-days (route-ttl-days state)))
        (multiple-value-bind (candidate-code candidate-text)
            (parse-agent-reply message)
          (let ((explicit-route (and candidate-code
                                     (find-route-by-code candidate-code
                                                         routes
                                                         ttl-days))))
            (if explicit-route
                (values explicit-route candidate-text nil)
                (if (route-code-token-p state candidate-code)
                    (values nil nil nil candidate-code)
                    (let ((default-route (last-active-route routes)))
                      (when default-route
                        (values default-route message t)))))))))))

(defun route-reply-success-message (route text default-route-p)
  (let ((suffix (if default-route-p " (default route)" "")))
    (if (plusp (length text))
        (format nil "xmpp-cli: sent feedback to ~a~a" (getf route :code) suffix)
        (format nil "xmpp-cli: focused ~a~a" (getf route :code) suffix))))

(defun handle-resolved-route-reply (state from route text default-route-p)
  (handler-case
      (progn
        (apply-route-reply route text)
        (mark-route-used route)
        (send-daemon-note
         state
         from
         (route-reply-success-message route text default-route-p)))
    (error (condition)
      (send-daemon-note
       state
       from
       (format nil "xmpp-cli: route ~a failed: ~a"
               (getf route :code)
               condition)))))

(defun handle-route-reply (state from body)
  (multiple-value-bind (route text default-route-p unknown-route-code)
      (resolve-route-reply state body)
    (cond
      (route
       (handle-resolved-route-reply state from route text default-route-p))
      (unknown-route-code
       (send-daemon-note
        state
        from
        (format nil "xmpp-cli: route code ~a is unknown or stale; wait for a new Codex notification or use a current route code."
                unknown-route-code)))
      (t
       (send-daemon-note
        state
        from
        "xmpp-cli: no active route; wait for a Codex notification or prepend a route code.")))))

(defun parse-agent-command (body)
  (let ((text (reply-text body)))
    (when (command-text-p text)
      (let ((without-slash (subseq text 1)))
        (multiple-value-bind (name rest)
            (parse-agent-reply without-slash)
          (values name rest))))))

(defun split-command-arguments (text)
  (let ((trimmed (reply-text text)))
    (when (plusp (length trimmed))
      (loop with parts = nil
            with start = 0
            with length = (length trimmed)
            while (< start length)
            for end = (position-if (lambda (char)
                                     (member char
                                             *reply-whitespace*
                                             :test #'char=))
                                   trimmed
                                   :start start)
            do (progn
                 (push (subseq trimmed start end) parts)
                 (setf start
                       (or (and end
                                (position-if-not
                                 (lambda (char)
                                   (member char
                                           *reply-whitespace*
                                           :test #'char=))
                                 trimmed
                                 :start end))
                           length)))
            finally (return (nreverse parts))))))

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun resolve-command-route (state route-code)
  (let* ((routes (active-routes-for-state state))
         (ttl-days (route-ttl-days state)))
    (if (and route-code (plusp (length route-code)))
        (find-route-by-code route-code routes ttl-days)
        (last-active-route routes))))

(defparameter *direct-route-command-handlers*
  (make-hash-table :test 'equal))

(defparameter *room-route-command-handlers*
  (make-hash-table :test 'equal))

(defun route-command-reply (state context text)
  (when (and text (plusp (length text)))
    (ecase (getf context :scope)
      (:direct
       (send-daemon-note state (getf context :from) text))
      (:room
       (send-room-note state (getf context :room-jid) text)))))

(defun route-command-arguments-valid-p (arguments min-args max-args)
  (and (>= (length arguments) min-args)
       (or (null max-args)
           (<= (length arguments) max-args))))

(defun route-command-usage (state context usage)
  (route-command-reply state context
                       (format nil "xmpp-cli: usage: ~a" usage)))

(defun route-command-failure-target (route room)
  (or (and route (getf route :code))
      (and room (getf room :route-code))
      "target"))

(defun invoke-route-command (state route room arguments context function)
  (handler-case
      (route-command-reply
       state
       context
       (funcall function state route room arguments context))
    (error (condition)
      (route-command-reply
       state
       context
       (format nil "xmpp-cli: /~a failed for ~a: ~a"
               (getf context :name)
               (route-command-failure-target route room)
               condition)))))

(defun dispatch-direct-route-command (state
                                      from
                                      arguments
                                      name
                                      direct-route
                                      target
                                      min-args
                                      max-args
                                      usage
                                      function)
  (multiple-value-bind (route-code command-arguments default-route-p)
      (ecase direct-route
        (:optional
         (values (first arguments) nil (null arguments)))
        (:required
         (values (first arguments) (rest arguments) nil)))
    (let ((context (list :scope :direct
                         :name name
                         :from from
                         :default-route-p default-route-p)))
      (cond
        ((and (eq direct-route :optional)
              (> (length arguments) 1))
         (route-command-usage state context usage))
        ((and (eq direct-route :required) (null route-code))
         (route-command-usage state context usage))
        ((not (route-command-arguments-valid-p command-arguments
                                               min-args
                                               max-args))
         (route-command-usage state context usage))
        ((eq target :route)
         (let ((route (resolve-command-route state route-code)))
           (cond
             ((null route)
              (route-command-reply
               state
               context
               (if route-code
                   (format nil "xmpp-cli: unknown route code ~a" route-code)
                   (format nil "xmpp-cli: no active route; wait for a Codex notification or pass ~a."
                           usage))))
             (t
              (invoke-route-command
               state route nil command-arguments context function)))))
        ((eq target :room)
         (let ((room (and route-code
                          (find-active-room-by-route-code route-code))))
           (cond
             ((null room)
              (route-command-reply
               state
               context
               (format nil "xmpp-cli: no active room matched ~a"
                       (or route-code "<missing>"))))
             (t
              (invoke-route-command
               state
               (route-for-room state room)
               room
               command-arguments
               context
               function)))))))))

(defun dispatch-room-route-command (state
                                    room
                                    arguments
                                    name
                                    target
                                    min-args
                                    max-args
                                    usage
                                    function)
  (let ((context (list :scope :room
                       :name name
                       :room room
                       :room-jid (getf room :room-jid))))
    (cond
      ((not (route-command-arguments-valid-p arguments min-args max-args))
       (route-command-usage state context usage))
      ((eq target :route)
       (let ((route (route-for-room state room)))
         (cond
           ((null route)
            (route-command-reply
             state
             context
             (format nil "xmpp-cli: route ~a is no longer active."
                     (getf room :route-code))))
           (t
            (invoke-route-command
             state route room arguments context function)))))
      ((eq target :room)
       (invoke-route-command
        state
        (route-for-room state room)
        room
        arguments
        context
        function)))))

(defmacro define-route-command (name options &body body)
  (let* ((base-name (string-downcase (symbol-name name)))
         (room-name (or (getf options :room-name) base-name))
         (direct-name-option (getf options :direct-name :same))
         (direct-name (cond
                        ((stringp direct-name-option)
                         direct-name-option)
                        ((eq direct-name-option :same)
                         room-name)
                        ((eq direct-name-option :room-prefixed)
                         (concatenate 'string "room-" room-name))
                        (t
                         (error "Unknown direct command name option ~s."
                                direct-name-option))))
         (direct-route (getf options :direct-route :required))
         (target (getf options :target :route))
         (min-args (getf options :min-args 0))
         (max-args (getf options :max-args 0))
         (direct-usage (or (getf options :direct-usage)
                           (format nil "/~a <route-code>" direct-name)))
         (room-usage (or (getf options :room-usage)
                         (format nil "/~a" room-name)))
         (core-name (intern (format nil "%~a-ROUTE-COMMAND"
                                    (string-upcase base-name))))
         (direct-handler-name (intern (format nil "%~a-DIRECT-COMMAND"
                                              (string-upcase base-name))))
         (room-handler-name (intern (format nil "%~a-ROOM-COMMAND"
                                            (string-upcase base-name)))))
    (unless (member direct-route '(:required :optional))
      (error "Unknown direct route policy ~s." direct-route))
    (unless (member target '(:route :room))
      (error "Unknown route command target ~s." target))
    (when (and (eq direct-route :optional)
               (or (plusp min-args)
                   (and max-args (plusp max-args))))
      (error "Optional direct route is only unambiguous for commands with no extra arguments."))
    (when (and (eq target :room)
               (eq direct-route :optional))
      (error "Room-targeted direct commands need an explicit route code."))
    `(progn
       (defun ,core-name (state route room arguments context)
         ,@body)
       (defun ,direct-handler-name (state from arguments)
         (dispatch-direct-route-command state
                                        from
                                        arguments
                                        ,direct-name
                                        ,direct-route
                                        ,target
                                        ,min-args
                                        ,max-args
                                        ,direct-usage
                                        #',core-name))
       (defun ,room-handler-name (state room arguments)
         (dispatch-room-route-command state
                                      room
                                      arguments
                                      ,room-name
                                      ,target
                                      ,min-args
                                      ,max-args
                                      ,room-usage
                                      #',core-name))
       (setf (gethash ,direct-name *direct-route-command-handlers*)
             #',direct-handler-name)
       (setf (gethash ,room-name *room-route-command-handlers*)
             #',room-handler-name)
       ',name)))

(defun plist-string (plist key)
  (let ((value (getf plist key)))
    (and (stringp value)
         (plusp (length value))
         value)))

(defun ensure-new-codex-route (state source-route new-context)
  (let* ((agent-session "")
         (host (plist-string source-route :host))
         (cwd (or (plist-string source-route :cwd)
                  (plist-string new-context :tmux-pane-current-path)))
         (display-cwd (if (and (string= (or cwd "")
                                        (or (getf source-route :cwd) ""))
                               (plist-string source-route :display-cwd))
                          (getf source-route :display-cwd)
                          (display-path cwd)))
         (socket (or (plist-string new-context :tmux-socket)
                     (plist-string source-route :tmux-socket)))
         (session-id (plist-string new-context :tmux-session-id))
         (window-id (plist-string new-context :tmux-window-id))
         (pane-id (plist-string new-context :tmux-pane-id))
         (identity (canonical-route-identity
                    :host host
                    :tmux-socket socket
                    :tmux-session-id session-id
                    :tmux-window-id window-id
                    :tmux-pane-id pane-id
                    :agent :codex
                    :agent-session agent-session)))
    (unless pane-id
      (error "New Codex pane context does not include a tmux pane id."))
    (unless cwd
      (error "New Codex pane route does not include a working directory."))
    (ensure-route identity
                  :code-length (getf (daemon-state-agent-config state)
                                     :code-length
                                     4)
                  :route-ttl-days (route-ttl-days state)
                  :agent :codex
                  :agent-session agent-session
                  :host host
                  :cwd cwd
                  :display-cwd display-cwd
                  :tmux-socket socket
                  :tmux-client-name (plist-string source-route
                                                  :tmux-client-name)
                  :tmux-client-tty (plist-string source-route
                                                 :tmux-client-tty)
                  :tmux-session-id session-id
                  :tmux-window-id window-id
                  :tmux-pane-id pane-id)))

(defun handle-new-command (state from arguments)
  (cond
    ((> (length arguments) 1)
     (send-daemon-note state from "xmpp-cli: usage: /new [route-code]"))
    (t
     (let* ((route-code (first arguments))
            (route (resolve-command-route state route-code)))
       (cond
         ((null route)
          (send-daemon-note
           state
           from
           (if route-code
               (format nil "xmpp-cli: unknown route code ~a" route-code)
               "xmpp-cli: no active route; wait for a Codex notification or pass /new <route-code>.")))
         (t
          (handler-case
              (let* ((new-context (start-codex-session route))
                     (updated-route (mark-route-used route))
                     (new-route (ensure-new-codex-route state
                                                        updated-route
                                                        new-context)))
                (send-daemon-note
                 state
                 from
                 (format nil "xmpp-cli: started new Codex session ~a from ~a in window ~a pane ~a"
                         (getf new-route :code)
                         (getf route :code)
                         (or (getf new-route :tmux-window-id) "unknown")
                         (getf new-route :tmux-pane-id))))
            (error (condition)
              (send-daemon-note
               state
               from
               (format nil "xmpp-cli: /new failed for ~a: ~a"
                       (getf route :code)
                       condition))))))))))

(defun boolean-config-value (value)
  (if value "1" "0"))

(defun requested-room-config-fields (config display-name)
  (list (list "FORM_TYPE" "http://jabber.org/protocol/muc#roomconfig" "hidden")
        (list "muc#roomconfig_roomname" (or display-name "xmpp-cli") "text-single")
        (list "muc#roomconfig_persistentroom"
              (boolean-config-value (getf config :room-persistent))
              "boolean")
        (list "muc#roomconfig_publicroom"
              (boolean-config-value (getf config :room-public))
              "boolean")
        (list "muc#roomconfig_membersonly"
              (boolean-config-value (getf config :room-members-only))
              "boolean")
        (list "muc#roomconfig_whois"
              (or (getf config :room-whois) "moderators")
              "list-single")
        (list "muc#roomconfig_moderatedroom"
              (boolean-config-value (getf config :room-moderated))
              "boolean")
        (list "muc#roomconfig_allowinvites"
              (boolean-config-value (getf config :room-allow-invites))
              "boolean")
        (list "muc#roomconfig_passwordprotectedroom" "0" "boolean")))

(defun returned-form-vars (fields)
  (remove nil (mapcar (lambda (field)
                        (getf field :var))
                      fields)))

(defun select-room-config-fields (returned-fields requested-fields)
  (let ((vars (returned-form-vars returned-fields)))
    (if vars
        (remove-if-not (lambda (field)
                         (member (first field) vars :test #'string=))
                       requested-fields)
        nil)))

(defun ensure-iq-ok (response label to)
  (cond
    ((null response)
     (error "~a to ~a returned no response." label to))
    ((string-equal (or (getf response :type) "") "error")
     (error "~a to ~a returned an XMPP error." label to))
    (t response)))

(defun request-room-config-fields (state room-jid)
  (let* ((backend (daemon-state-backend state))
         (response
           (send-iq-and-wait
            state
            (lambda (connection id)
              (request-room-config backend connection room-jid id)))))
    (getf (ensure-iq-ok response "room config request" room-jid)
          :data-form-fields)))

(defun submit-room-config-fields (state room-jid fields)
  (let ((backend (daemon-state-backend state)))
    (ensure-iq-ok
     (send-iq-and-wait
      state
      (lambda (connection id)
        (submit-room-config backend connection room-jid id fields)))
     "room config submit"
     room-jid)))

(defun grant-room-member-and-wait (state room-jid jid)
  (let ((backend (daemon-state-backend state)))
    (ensure-iq-ok
     (send-iq-and-wait
      state
      (lambda (connection id)
        (grant-room-membership backend connection room-jid id jid)))
     "room member grant"
     room-jid)))

(defun send-direct-room-invite-from-state (state to room-jid reason)
  (let ((backend (daemon-state-backend state))
        (connection (current-xmpp-connection-or-error state)))
    (call-with-xmpp-write-lock
     state
     (lambda ()
       (send-direct-room-invite backend connection to room-jid reason)))))

(defun invite-room-jids (state jids room-jid reason)
  (dolist (jid jids)
    (send-direct-room-invite-from-state state jid room-jid reason)))

(defun join-room-created-p (presence)
  (member "201" (getf presence :muc-status-codes) :test #'string=))

(defun join-room-error-p (presence)
  (string-equal "error" (getf presence :type)))

(defun leave-room-from-state (state room-full-jid)
  (let ((backend (daemon-state-backend state))
        (connection (current-xmpp-connection-or-error state)))
    (call-with-xmpp-write-lock
     state
     (lambda ()
       (leave-room backend connection room-full-jid)))))

(defun room-invitees (state from)
  (let* ((sender (bare-jid from))
         (notify-to (getf (daemon-state-agent-config state) :notify-to)))
    (remove-duplicates
     (remove nil
             (list sender
                   (and notify-to
                        (not (string-equal notify-to sender))
                        notify-to)))
     :test #'string-equal)))

(defun configure-room (state room-jid display-name)
  (let* ((config (daemon-state-agent-config state))
         (returned-fields (request-room-config-fields state room-jid))
         (fields (select-room-config-fields
                  returned-fields
                  (requested-room-config-fields config display-name))))
    (submit-room-config-fields state room-jid fields)
    fields))

(defun existing-route-room-message (room)
  (format nil "xmpp-cli: route ~a already has room~%room: ~a~%join: ~a"
          (getf room :route-code)
          (getf room :room-jid)
          (join-uri (getf room :room-jid))))

(defun create-room-binding-locked (state from route room-name)
  (let ((existing (find-active-room-by-route-id (getf route :route-id))))
    (when existing
      (return-from create-room-binding-locked (values existing t))))
  (let* ((service-result (daemon-discover-muc-service state))
         (service-jid (getf service-result :service-jid))
         (route-code (getf route :code))
         (room-jid nil)
         (room-full nil)
         (join-presence nil)
         (nick (getf (daemon-state-agent-config state) :room-nick "xmpp-cli"))
         (now (now-iso8601))
         (invitees (room-invitees state from)))
    (loop repeat 3
          do (progn
               (setf room-jid (make-room-jid service-jid route-code room-name))
               (setf room-full (room-full-jid room-jid nick))
               (setf join-presence (join-room-and-wait state room-full))
               (cond
                 ((join-room-error-p join-presence)
                  (error "room join returned an XMPP error for ~a." room-jid))
                 ((join-room-created-p join-presence)
                  (return))
                 (t
                  (ignore-errors
                    (leave-room-from-state state room-full))
                  (setf room-jid nil
                        room-full nil
                        join-presence nil))))
          finally (unless room-jid
                    (error "could not create a new room after several name attempts.")))
    (configure-room state room-jid room-name)
    (dolist (jid invitees)
      (grant-room-member-and-wait state room-jid jid))
    (let ((room (list :room-jid room-jid
                      :room-nick nick
                      :route-id (getf route :route-id)
                      :route-code route-code
                      :service-jid service-jid
                      :room-name (or room-name "room")
                      :invited-jids invitees
                      :created-at now
                      :last-activity-at now
                      :state "active")))
      (upsert-room room)
      (invite-room-jids
       state
       invitees
       room-jid
       (format nil "xmpp-cli room for route ~a" route-code))
      (values room nil))))

(defun create-room-binding (state from route room-name)
  ;; Keep the remote MUC creation and local persistence as one critical section.
  ;; The rooms file lock only protects writes after a room exists; without this
  ;; lock, concurrent /room commands can both create remote rooms for one route.
  (bt:with-lock-held (*room-binding-lock*)
    (create-room-binding-locked state from route room-name)))

(defun room-created-message (room)
  (format nil "xmpp-cli: created room for route ~a~%room: ~a~%join: ~a"
          (getf room :route-code)
          (getf room :room-jid)
          (join-uri (getf room :room-jid))))

(defun handle-room-command-worker (state from route room-name)
  (handler-case
      (multiple-value-bind (room existing-p)
          (create-room-binding state from route room-name)
        (send-daemon-note
         state
         from
         (if existing-p
             (existing-route-room-message room)
             (room-created-message room))))
    (error (condition)
      (send-daemon-note
       state
       from
       (format nil "xmpp-cli: /room failed: ~a" condition)))))

(defun start-room-command-worker (state from route room-name)
  (bt:make-thread
   (lambda ()
     (handle-room-command-worker state from route room-name))
   :name "xmpp-cli room command"))

(defun parse-room-command-arguments (text)
  (multiple-value-bind (route-code room-name)
      (parse-agent-reply text)
    (values route-code
            (and room-name
                 (plusp (length room-name))
                 room-name))))

(defun handle-room-command (state from rest)
  (multiple-value-bind (route-code room-name)
      (parse-room-command-arguments rest)
    (cond
      ((or (null route-code) (zerop (length route-code)))
       (send-daemon-note state from "xmpp-cli: usage: /room <route-code> [room-name]"))
      ((not (route-code-token-p state route-code))
       (send-daemon-note
        state
        from
        (format nil "xmpp-cli: invalid route code ~a" route-code)))
      (t
       (let ((route (resolve-command-route state route-code)))
         (if route
             (let ((existing (find-active-room-by-route-id
                              (getf route :route-id))))
               (if existing
                   (send-daemon-note
                    state
                    from
                    (existing-route-room-message existing))
                   (progn
                     (send-daemon-note
                      state
                      from
                      (format nil "xmpp-cli: creating room for route ~a..."
                              (getf route :code)))
                     (start-room-command-worker state from route room-name))))
             (send-daemon-note
              state
              from
              (format nil "xmpp-cli: unknown route code ~a" route-code))))))))

(defun room-message-from-self-p (room stanza)
  (let ((nick (getf stanza :room-nick))
        (bot-nick (getf room :room-nick)))
    (and nick bot-nick (string= nick bot-nick))))

(defun room-message-sender-jid (state room stanza)
  (let* ((nick (getf stanza :room-nick))
         (occupant (and nick
                        (room-occupant state
                                       (getf room :room-jid)
                                       nick)))
         (jid (and occupant (getf occupant :jid))))
    (and jid (bare-jid jid))))

(defun authorized-room-sender-p (state room stanza)
  (let ((sender (room-message-sender-jid state room stanza)))
    (and sender
         (member sender
                 (allowed-senders (daemon-state-agent-config state))
                 :test #'string-equal)
         sender)))

(defun room-has-known-allowed-occupant-p (state room)
  "Return true when an allowed non-bot JID is currently known in ROOM.

MUC messages are delivered to occupants, not to every affiliated member. The
daemon uses this as a delivery guard so route notifications do not disappear
into a room that only the bot has joined after reconnect."
  (let ((room-jid (getf room :room-jid))
        (allowed (allowed-senders (daemon-state-agent-config state)))
        (bot-jid (bare-jid (getf (daemon-state-profile state) :jid)))
        (found nil))
    (when (and room-jid allowed)
      (bt:with-lock-held ((daemon-state-lock state))
        (let ((occupants (room-occupants-table state room-jid)))
          (when occupants
            (maphash
             (lambda (nick occupant)
               (declare (ignore nick))
               (let ((jid (bare-jid (getf occupant :jid))))
                 (when (and (plusp (length jid))
                            (not (string-equal jid bot-jid))
                            (member jid allowed :test #'string-equal))
                   (setf found t))))
             occupants)))))
    found))

(defun route-for-room (state room)
  (find-route-by-id
   (active-routes-for-state state)
   (getf room :route-id)))

(defun handle-room-route-success (state room route text sender)
  (declare (ignore sender))
  (apply-route-reply route text)
  (mark-route-used route :direct-p nil)
  (mark-room-activity room)
  (send-room-note
   state
   (getf room :room-jid)
   (format nil "xmpp-cli: sent feedback to ~a" (getf route :code))))

(defun focus-route-for-command (route &key (direct-p t) room default-route-p)
  (focus-pane route)
  (mark-route-used route :direct-p direct-p)
  (when room
    (mark-room-activity room))
  (format nil "xmpp-cli: focused ~a~a"
          (getf route :code)
          (if default-route-p " (default route)" "")))

(define-route-command focus
    (:direct-name :same
     :room-name "focus"
     :direct-route :optional
     :target :route
     :min-args 0
     :max-args 0
     :direct-usage "/focus [route-code]"
     :room-usage "/focus")
  (declare (ignore arguments))
  (focus-route-for-command route
                           :direct-p (eq (getf context :scope) :direct)
                           :room room
                           :default-route-p (getf context :default-route-p)))

(defun room-close-result-message (room destroyed destroy-error)
  (if destroyed
      (format nil "xmpp-cli: closed room ~a"
              (getf room :room-jid))
      (format nil
              "xmpp-cli: closed room ~a locally; server destroy was not acknowledged: ~a"
              (getf room :room-jid)
              destroy-error)))

(define-route-command close
    (:direct-name :room-prefixed
     :room-name "close"
     :direct-route :required
     :target :room
     :min-args 0
     :max-args 0
     :direct-usage "/room-close <route-code>"
     :room-usage "/close")
  (declare (ignore route arguments))
  (let ((room-jid (getf room :room-jid)))
    (when (eq (getf context :scope) :room)
      (route-command-reply
       state
       context
       (format nil "xmpp-cli: closing room ~a" room-jid)))
    (multiple-value-bind (destroyed destroy-error)
        (close-room state
                    room
                    :reason (if (eq (getf context :scope) :room)
                                "closed by xmpp-cli room user"
                                "closed by xmpp-cli user"))
      (if (eq (getf context :scope) :direct)
          (room-close-result-message room destroyed destroy-error)
          (progn
            (when destroy-error
              (format *error-output*
                      "~&xmpp-cli daemon: closed room ~a locally; server destroy was not acknowledged: ~a~%"
                      room-jid
                      destroy-error)
              (finish-output *error-output*))
            nil)))))

(defun handle-room-local-command (state room body)
  (multiple-value-bind (name rest)
      (parse-agent-command body)
    (when name
      (let ((room-jid (getf room :room-jid))
            (handler (gethash name *room-route-command-handlers*)))
        (cond
          (handler
           (funcall handler state room (split-command-arguments rest))
           t)
          ((string-prefix-p "room-" name)
           (send-room-note
            state
            room-jid
            (format nil
                    "xmpp-cli: room commands inside a room omit the room- prefix; use /~a"
                    (subseq name (length "room-"))))
           t)
          (t
           (send-room-note
            state
            room-jid
            (format nil "xmpp-cli: unknown room command /~a" name))
           t))))))

(defun handle-room-message (state stanza)
  (let* ((body (reply-text (getf stanza :body)))
         (room-jid (getf stanza :room-jid)))
    (when (and room-jid (plusp (length body)))
      (let ((room (find-active-room-by-jid room-jid)))
        (cond
          ((null room)
           nil)
          ((room-message-from-self-p room stanza)
           nil)
          (t
           (let ((sender (authorized-room-sender-p state room stanza)))
             (cond
               ((null sender)
                (send-room-note
                 state
                 (getf room :room-jid)
                 "xmpp-cli: ignored message because the room sender could not be verified as an allowed JID."))
               ((handle-room-local-command state room body)
                t)
               (t
                (let ((route (route-for-room state room)))
                  (cond
                    ((null route)
                     (send-room-note
                      state
                      (getf room :room-jid)
                      (format nil "xmpp-cli: route ~a is no longer active."
                              (getf room :route-code))))
                    (t
                     (handler-case
                         (handle-room-route-success state room route body sender)
                       (error (condition)
                         (send-room-note
                          state
                          (getf room :room-jid)
                          (format nil "xmpp-cli: route ~a failed: ~a"
                                  (getf room :route-code)
                                  condition))))))))))))))))

(defun room-full-jid-for-room (state room)
  (room-full-jid (getf room :room-jid)
                 (or (getf room :room-nick)
                     (getf (daemon-state-agent-config state)
                           :room-nick
                           "xmpp-cli"))))

(defun destroy-room-and-wait (state room &key reason)
  (let* ((room-jid (getf room :room-jid))
         (backend (daemon-state-backend state)))
    (ensure-iq-ok
     (send-iq-and-wait
      state
      (lambda (connection id)
        (destroy-room backend connection room-jid id :reason reason)))
     "room destroy"
     room-jid)))

(defun close-room (state room &key reason (mark-on-destroy-failure t))
  (let ((room-full (room-full-jid-for-room state room))
        (destroy-error nil))
    (handler-case
        (destroy-room-and-wait state room :reason reason)
      (error (condition)
        (setf destroy-error condition)))
    (cond
      ((or (null destroy-error) mark-on-destroy-failure)
       (ignore-errors
         (leave-room-from-state state room-full))
       (mark-room-closed room))
      (t
       (error destroy-error)))
    (values (null destroy-error) destroy-error)))

(defun rejoin-room (state room)
  (let* ((room-jid (getf room :room-jid))
         (nick (or (getf room :room-nick)
                   (getf (daemon-state-agent-config state)
                         :room-nick
                         "xmpp-cli")))
         (presence (join-room-and-wait state (room-full-jid room-jid nick))))
    (when (join-room-error-p presence)
      (error "room rejoin returned an XMPP error for ~a." room-jid))
    (when (join-room-created-p presence)
      (configure-room state room-jid (getf room :room-name))
      (dolist (jid (getf room :invited-jids))
        (grant-room-member-and-wait state room-jid jid))
      (invite-room-jids
       state
       (getf room :invited-jids)
       room-jid
       (format nil "xmpp-cli room for route ~a"
               (getf room :route-code))))
    presence))

(defun rejoin-active-rooms (state)
  (dolist (room (active-rooms))
    (handler-case
        (progn
          (rejoin-room state room)
          (format *error-output*
                  "~&xmpp-cli daemon: rejoined room ~a~%"
                  (getf room :room-jid)))
      (error (condition)
        (format *error-output*
                "~&xmpp-cli daemon: could not rejoin room ~a: ~a~%"
                (getf room :room-jid)
                condition))))
  (finish-output *error-output*))

(defun start-room-rejoin-worker (state)
  (bt:make-thread
   (lambda ()
     (rejoin-active-rooms state))
   :name "xmpp-cli room rejoin"))

(defun stale-room-reason (state route room)
  (cond
    ((null route)
     (format nil "route ~a expired or no longer exists"
             (getf room :route-code)))
    ((room-expired-p room
                     (getf (daemon-state-agent-config state)
                           :room-ttl-hours))
     (format nil "room exceeded room_ttl_hours (~d)"
             (getf (daemon-state-agent-config state)
                   :room-ttl-hours)))
    ((not (pane-exists-p route))
     (format nil "route ~a tmux pane no longer exists"
             (getf room :route-code)))
    (t nil)))

(defun cleanup-stale-rooms (state)
  (let* ((routes (active-routes-for-state state))
         (closed 0))
    (dolist (room (active-rooms) closed)
      (let* ((route (find-route-by-id routes (getf room :route-id)))
             (reason (stale-room-reason state route room)))
        (when reason
          (handler-case
              (progn
                (close-room state room
                            :reason reason
                            :mark-on-destroy-failure nil)
                (incf closed)
                (format *error-output*
                        "~&xmpp-cli daemon: closed room ~a: ~a~%"
                        (getf room :room-jid)
                        reason))
            (error (condition)
              (format *error-output*
                      "~&xmpp-cli daemon: could not close stale room ~a: ~a~%"
                      (getf room :room-jid)
                      condition))))))))

(defun handle-rooms-command (state from arguments)
  (if arguments
      (send-daemon-note state from "xmpp-cli: usage: /rooms")
      (let ((lines (room-summary-lines)))
        (send-daemon-note
         state
         from
         (if lines
             (format nil "xmpp-cli rooms:~%~{~a~%~}" lines)
             "xmpp-cli: no active rooms")))))

(defun handle-direct-command (state from body)
  (multiple-value-bind (name rest)
      (parse-agent-command body)
    (let ((handler (and name
                        (gethash name *direct-route-command-handlers*))))
      (cond
        ((null name)
         nil)
        (handler
         (funcall handler state from (split-command-arguments rest)))
        ((string= name "new")
         (handle-new-command state from (split-command-arguments rest)))
        ((string= name "room")
         (handle-room-command state from rest))
        ((string= name "rooms")
         (handle-rooms-command state from (split-command-arguments rest)))
        (t
         (send-daemon-note
          state
          from
          (format nil "xmpp-cli: unknown command /~a" name)))))))

(defun log-message-error (message)
  (let ((from (getf message :from))
        (body (getf message :body)))
    (format *error-output*
            "~&xmpp-cli daemon: received XMPP message error from ~a~@[ body=~a~]~%"
            (or from "<unknown>")
            body)
    (finish-output *error-output*)))

(defun handle-incoming-message (state message)
  (let ((from (getf message :from))
        (body (getf message :body)))
    (when (string-equal "error" (or (getf message :type) ""))
      (log-message-error message))
    (when (and body (plusp (length body)))
      (cond
        ((not (authorized-sender-p state from))
         (format *error-output*
                 "~&xmpp-cli daemon: ignored message from unauthorized sender ~a~%"
                 (or from "<unknown>")))
        (t
         (let ((text (reply-text body)))
           (if (command-text-p text)
               (handle-direct-command state from text)
               (handle-route-reply state from text))))))))
