(in-package #:xmpp-cli/agent-config)

(defparameter *default-reconnect-backoff-seconds* '(1 2 5 10 30 60 300))

(defparameter *agent-config-defaults*
  (list :profile "default"
        :notify-to nil
        :allowed-senders nil
        :daemon-resource "xmpp-agent-helper"
        :code-length 4
        :route-ttl-days 90
        :reconnect-backoff-seconds *default-reconnect-backoff-seconds*
        :muc-service nil
        :muc-discovery t
        :muc-discovery-cache-hours 24
        :room-nick "xmpp-cli"
        :room-public nil
        :room-persistent nil
        :room-members-only t
        :room-whois "moderators"
        :room-moderated nil
        :room-allow-invites nil))

(defun agent-directory ()
  (merge-pathnames "agent/" (home-xmpp-cli-directory)))

(defun agent-config-pathname ()
  (merge-pathnames "config.yaml" (agent-directory)))

(defun empty-agent-config ()
  (copy-list *agent-config-defaults*))

(defun ensure-agent-directory ()
  (ensure-private-directory (agent-directory)))

(defun validate-jid (jid label)
  (handler-case
      (progn
        (split-jid jid)
        jid)
    (error (condition)
      (error "~a must be a bare JID: ~a" label condition))))

(defun validate-service-jid (jid label)
  (unless (and (stringp jid)
               (plusp (length jid))
               (not (position #\/ jid))
               (not (find-if (lambda (char)
                               (member char '(#\Space #\Tab #\Newline #\Return)
                                       :test #'char=))
                             jid)))
    (error "~a must be a non-empty XMPP service JID without a resource: ~a"
           label
           jid))
  jid)

(defun validate-boolean (value label)
  (unless (or (eq value t) (null value))
    (error "agent/config.yaml ~a must be true or false." label))
  value)

(defun validate-non-empty-string (value label)
  (unless (and (stringp value) (plusp (length value)))
    (error "agent/config.yaml ~a must be a non-empty string." label))
  value)

(defun validate-positive-integer (value label)
  (unless (and (integerp value) (plusp value))
    (error "agent/config.yaml ~a must be a positive integer." label))
  value)

(defun validate-positive-integer-list (value label)
  (unless (and (listp value)
               (every (lambda (item)
                        (and (integerp item) (plusp item)))
                      value))
    (error "agent/config.yaml ~a must be a list of positive integers." label))
  value)

(defun validate-optional-jid (value label)
  (when value
    (validate-jid value label))
  value)

(defun validate-allowed-senders (value label)
  (unless (listp value)
    (error "agent/config.yaml ~a must be a list." label))
  (dolist (sender value)
    (validate-jid sender "allowed sender"))
  value)

(defun validate-optional-service-jid (value label)
  (when value
    (validate-service-jid value label))
  value)

(defun validate-room-whois (value label)
  (unless (and (stringp value)
               (member value '("moderators" "anyone") :test #'string-equal))
    (error "agent/config.yaml ~a must be moderators or anyone." label))
  value)

(defparameter *agent-config-fields*
  '((:profile "profile" validate-non-empty-string)
    (:notify-to "notify_to" validate-optional-jid :nullable)
    (:allowed-senders "allowed_senders" validate-allowed-senders :list)
    (:daemon-resource "daemon_resource" validate-non-empty-string)
    (:code-length "code_length" validate-positive-integer)
    (:route-ttl-days "route_ttl_days" validate-positive-integer)
    (:reconnect-backoff-seconds
     "reconnect_backoff_seconds"
     validate-positive-integer-list
     :list)
    (:muc-service "muc_service" validate-optional-service-jid :nullable)
    (:muc-discovery "muc_discovery" validate-boolean :boolean)
    (:muc-discovery-cache-hours
     "muc_discovery_cache_hours"
     validate-positive-integer)
    (:room-nick "room_nick" validate-non-empty-string)
    (:room-public "room_public" validate-boolean :boolean)
    (:room-persistent "room_persistent" validate-boolean :boolean)
    (:room-members-only "room_members_only" validate-boolean :boolean)
    (:room-whois "room_whois" validate-room-whois)
    (:room-moderated "room_moderated" validate-boolean :boolean)
    (:room-allow-invites "room_allow_invites" validate-boolean :boolean)))

(defun config-field-key (field)
  (first field))

(defun config-field-yaml-key (field)
  (second field))

(defun config-field-validator (field)
  (third field))

(defun config-field-kind (field)
  (or (fourth field) :plain))

(defun validate-agent-config (config)
  (unless (and (listp config) (evenp (length config)))
    (error "Malformed agent config: expected a property list."))
  (dolist (field *agent-config-fields*)
    (funcall (config-field-validator field)
             (getf config (config-field-key field))
             (config-field-yaml-key field)))
  config)

(defun yaml-boolean (value)
  (if value t (yaml-false)))

(defun config-value-to-yaml (value kind)
  (case kind
    (:nullable (or value (xmpp-cli/yaml:yaml-null)))
    (:boolean (yaml-boolean value))
    (:list (copy-list value))
    (t value)))

(defun config-to-yaml (config)
  (validate-agent-config config)
  (loop for field in *agent-config-fields*
        collect (cons (config-field-yaml-key field)
                      (config-value-to-yaml
                       (getf config (config-field-key field))
                       (config-field-kind field)))))

(defun agent-config-as-yaml (config)
  (config-to-yaml config))

(defun yaml-entry-present-p (yaml key)
  (and (assoc key yaml :test #'string=) t))

(defun required-config-yaml-value (yaml key)
  (unless (yaml-entry-present-p yaml key)
    (error "Malformed agent/config.yaml: missing required key ~a." key))
  (yaml-value yaml key nil))

(defun yaml-to-config (yaml)
  (unless (listp yaml)
    (error "Malformed agent/config.yaml: expected a mapping."))
  (let ((config (loop for field in *agent-config-fields*
                      append (list (config-field-key field)
                                   (required-config-yaml-value
                                    yaml
                                    (config-field-yaml-key field))))))
    (validate-agent-config config)))

(defun load-agent-config ()
  (let ((pathname (agent-config-pathname)))
    (if (probe-file pathname)
        (yaml-to-config (read-yaml-file pathname))
        (empty-agent-config))))

(defun save-agent-config (config)
  (validate-agent-config config)
  (ensure-agent-directory)
  (write-yaml-atomically (agent-config-pathname) (config-to-yaml config)))

(defun notify-to (config)
  (getf config :notify-to))

(defun set-notify-to (config jid)
  (validate-jid jid "notify_to")
  (let* ((senders (getf config :allowed-senders))
         (updated-senders (if senders senders (list jid)))
         (updated (copy-list config)))
    (setf (getf updated :notify-to) jid)
    (setf (getf updated :allowed-senders) updated-senders)
    (validate-agent-config updated)))

(defun allowed-senders (config)
  (getf config :allowed-senders))

(defun add-allowed-sender (config jid)
  (validate-jid jid "allowed sender")
  (let* ((senders (allowed-senders config))
         (updated (adjoin jid senders :test #'string=)))
    (setf (getf config :allowed-senders) updated)
    (validate-agent-config config)))

(defun remove-allowed-sender (config jid)
  (let ((updated (remove jid (allowed-senders config) :test #'string=)))
    (setf (getf config :allowed-senders) updated)
    (validate-agent-config config)))
