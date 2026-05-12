(in-package #:xmpp-cli/agent-config)

(defparameter *default-reconnect-backoff-seconds* '(1 2 5 10 30 60 300))

(defun agent-directory ()
  (merge-pathnames "agent/" (home-xmpp-cli-directory)))

(defun agent-config-pathname ()
  (merge-pathnames "config.yaml" (agent-directory)))

(defun empty-agent-config ()
  (list :profile "default"
        :notify-to nil
        :allowed-senders nil
        :daemon-resource "xmpp-agent-helper"
        :code-length 4
        :route-ttl-days 90
        :reconnect-backoff-seconds *default-reconnect-backoff-seconds*))

(defun ensure-agent-directory ()
  (ensure-private-directory (agent-directory)))

(defun validate-jid (jid label)
  (handler-case
      (progn
        (split-jid jid)
        jid)
    (error (condition)
      (error "~a must be a bare JID: ~a" label condition))))

(defun validate-agent-config (config)
  (unless (and (listp config) (evenp (length config)))
    (error "Malformed agent config: expected a property list."))
  (let ((profile (getf config :profile))
        (notify-to (getf config :notify-to))
        (allowed-senders (getf config :allowed-senders))
        (resource (getf config :daemon-resource))
        (code-length (getf config :code-length))
        (ttl (getf config :route-ttl-days))
        (backoff (getf config :reconnect-backoff-seconds)))
    (unless (and (stringp profile) (plusp (length profile)))
      (error "agent/config.yaml profile must be a non-empty string."))
    (when notify-to
      (validate-jid notify-to "notify_to"))
    (unless (listp allowed-senders)
      (error "agent/config.yaml allowed_senders must be a list."))
    (dolist (sender allowed-senders)
      (validate-jid sender "allowed sender"))
    (unless (and (stringp resource) (plusp (length resource)))
      (error "agent/config.yaml daemon_resource must be a non-empty string."))
    (unless (and (integerp code-length) (plusp code-length))
      (error "agent/config.yaml code_length must be a positive integer."))
    (unless (and (integerp ttl) (plusp ttl))
      (error "agent/config.yaml route_ttl_days must be a positive integer."))
    (unless (and (listp backoff)
                 (every (lambda (seconds)
                          (and (integerp seconds) (plusp seconds)))
                        backoff))
      (error "agent/config.yaml reconnect_backoff_seconds must be a list of positive integers.")))
  config)

(defun config-to-yaml (config)
  (validate-agent-config config)
  (list (cons "profile" (getf config :profile))
        (cons "notify_to" (or (getf config :notify-to) (xmpp-cli/yaml:yaml-null)))
        (cons "allowed_senders" (copy-list (getf config :allowed-senders)))
        (cons "daemon_resource" (getf config :daemon-resource))
        (cons "code_length" (getf config :code-length))
        (cons "route_ttl_days" (getf config :route-ttl-days))
        (cons "reconnect_backoff_seconds"
              (copy-list (getf config :reconnect-backoff-seconds)))))

(defun agent-config-as-yaml (config)
  (config-to-yaml config))

(defun yaml-to-config (yaml)
  (unless (listp yaml)
    (error "Malformed agent/config.yaml: expected a mapping."))
  (let* ((defaults (empty-agent-config))
         (notify-to (yaml-value yaml "notify_to" nil))
         (allowed-entry (assoc "allowed_senders" yaml :test #'string=))
         (allowed (and allowed-entry
                       (yaml-value yaml "allowed_senders" nil)))
         (config (list :profile (yaml-value yaml "profile" (getf defaults :profile))
                       :notify-to notify-to
                       :allowed-senders (if allowed-entry
                                            allowed
                                            (and notify-to (list notify-to)))
                       :daemon-resource (yaml-value yaml
                                                    "daemon_resource"
                                                    (getf defaults :daemon-resource))
                       :code-length (yaml-value yaml
                                                "code_length"
                                                (getf defaults :code-length))
                       :route-ttl-days (yaml-value yaml
                                                   "route_ttl_days"
                                                   (getf defaults :route-ttl-days))
                       :reconnect-backoff-seconds
                       (yaml-value yaml
                                   "reconnect_backoff_seconds"
                                   (getf defaults :reconnect-backoff-seconds)))))
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
         (updated-senders (if senders
                              senders
                              (list jid))))
    (validate-agent-config
     (list :profile (getf config :profile "default")
           :notify-to jid
           :allowed-senders updated-senders
           :daemon-resource (getf config :daemon-resource "xmpp-agent-helper")
           :code-length (getf config :code-length 4)
           :route-ttl-days (getf config :route-ttl-days 90)
           :reconnect-backoff-seconds
           (getf config
                 :reconnect-backoff-seconds
                 *default-reconnect-backoff-seconds*)))))

(defun allowed-senders (config)
  (or (getf config :allowed-senders)
      (let ((target (notify-to config)))
        (and target (list target)))))

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
