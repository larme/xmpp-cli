(in-package #:xmpp-cli/agent-muc)

(defparameter +muc-feature+ "http://jabber.org/protocol/muc")
(defparameter +disco-info-xmlns+ "http://jabber.org/protocol/disco#info")
(defparameter +disco-items-xmlns+ "http://jabber.org/protocol/disco#items")

(defun muc-services-pathname ()
  (merge-pathnames "muc-services.yaml" (agent-directory)))

(defun yaml-to-cache-entry (mapping)
  (unless (listp mapping)
    (error "Malformed agent/muc-services.yaml: service entry must be a mapping."))
  (list :account-domain (yaml-value mapping "account_domain" nil)
        :service-jid (yaml-value mapping "service_jid" nil)
        :discovered-at (yaml-value mapping "discovered_at" nil)
        :discovered-time (yaml-value mapping "discovered_time" nil)))

(defun cache-entry-to-yaml (entry)
  (list (cons "account_domain" (getf entry :account-domain))
        (cons "service_jid" (getf entry :service-jid))
        (cons "discovered_at" (getf entry :discovered-at))
        (cons "discovered_time" (getf entry :discovered-time))))

(defun load-muc-service-cache ()
  (read-yaml-record-list-file (muc-services-pathname)
                              #'yaml-to-cache-entry
                              :label "agent/muc-services.yaml"))

(defun save-muc-service-cache (entries)
  (ensure-agent-directory)
  (write-yaml-record-list-file (muc-services-pathname)
                               entries
                               #'cache-entry-to-yaml))

(defun cache-muc-service (account-domain service-jid &optional
                                         (now (get-universal-time)))
  (let* ((entries (load-muc-service-cache))
         (entry (list :account-domain account-domain
                      :service-jid service-jid
                      :discovered-at (now-iso8601 now)
                      :discovered-time now))
         (updated (cons entry
                        (remove account-domain
                                entries
                                :test #'string-equal
                                :key (lambda (candidate)
                                       (getf candidate :account-domain))))))
    (save-muc-service-cache updated)
    entry))

(defun cache-entry-fresh-p (entry cache-hours now)
  (let ((discovered-time (getf entry :discovered-time)))
    (and (integerp discovered-time)
         (plusp cache-hours)
         (<= (- now discovered-time)
             (* cache-hours 60 60)))))

(defun cached-muc-service (account-domain cache-hours &optional
                                         (now (get-universal-time)))
  (find-if (lambda (entry)
             (and (string-equal account-domain
                                (getf entry :account-domain))
                  (cache-entry-fresh-p entry cache-hours now)))
           (load-muc-service-cache)))

(defun muc-service-candidate-p (candidate)
  (member +muc-feature+
          (getf candidate :features)
          :test #'string=))

(defun conference-text-candidate-p (candidate)
  (find-if (lambda (identity)
             (and (string-equal "conference" (getf identity :category))
                  (string-equal "text" (getf identity :type))))
           (getf candidate :identities)))

(defun preferred-service-jid (account-domain)
  (format nil "conference.~a" account-domain))

(defun select-muc-service (account-domain candidates)
  "Return selected service plist, status keyword, and considered candidates."
  (let* ((muc-candidates (remove-if-not #'muc-service-candidate-p candidates))
         (identity-candidates (remove-if-not #'conference-text-candidate-p
                                             muc-candidates))
         (considered (if identity-candidates
                         identity-candidates
                         muc-candidates))
         (preferred (find (preferred-service-jid account-domain)
                          considered
                          :test #'string-equal
                          :key (lambda (candidate)
                                 (getf candidate :jid)))))
    (cond
      ((null muc-candidates)
       (values nil :none nil))
      ((= (length considered) 1)
       (values (first considered) :ok considered))
      (preferred
       (values preferred :ok considered))
      (t
       (values nil :ambiguous considered)))))

(defun candidate-jids (candidates)
  (mapcar (lambda (candidate)
            (or (getf candidate :jid) "<unknown>"))
          candidates))

(defun discover-candidates (account-domain request-items request-info)
  (loop for item in (funcall request-items account-domain)
        for jid = (getf item :jid)
        when (and (stringp jid) (plusp (length jid)))
          collect
          (handler-case
              (let ((info (funcall request-info jid)))
                (append (list :jid jid
                              :name (getf item :name))
                        info))
            (error (condition)
              (list :jid jid
                    :name (getf item :name)
                    :error (princ-to-string condition))))))

(defun configured-service-result (account-domain service-jid request-info)
  (let ((info (funcall request-info service-jid)))
    (unless (muc-service-candidate-p info)
      (error "Configured MUC service ~a does not advertise ~a."
             service-jid
             +muc-feature+))
    (list :account-domain account-domain
          :service-jid service-jid
          :source :configured
          :candidates (list (append (list :jid service-jid) info)))))

(defun resolve-muc-service (account-domain config request-items request-info
                            &key force (now (get-universal-time)))
  "Resolve an account domain to a MUC service using config, cache, or disco."
  (let ((configured (getf config :muc-service)))
    (when (and configured (not force))
      (return-from resolve-muc-service
        (configured-service-result account-domain configured request-info))))
  (let ((cache-hours (getf config :muc-discovery-cache-hours 24)))
    (unless force
      (let ((cached (cached-muc-service account-domain cache-hours now)))
        (when cached
          (return-from resolve-muc-service
            (list :account-domain account-domain
                  :service-jid (getf cached :service-jid)
                  :source :cache
                  :candidates nil))))))
  (unless (getf config :muc-discovery t)
    (error "MUC discovery is disabled and muc_service is not configured."))
  (let ((candidates (discover-candidates account-domain
                                         request-items
                                         request-info)))
    (multiple-value-bind (selected status considered)
        (select-muc-service account-domain candidates)
      (case status
        (:ok
         (let ((jid (getf selected :jid)))
           (cache-muc-service account-domain jid now)
           (list :account-domain account-domain
                 :service-jid jid
                 :source :discovered
                 :candidates candidates)))
        (:none
         (error "No MUC service advertising ~a was discovered for ~a."
                +muc-feature+
                account-domain))
        (:ambiguous
         (error "Multiple MUC services discovered for ~a: ~{~a~^, ~}. Set muc_service in agent/config.yaml."
                account-domain
                (candidate-jids considered)))
        (otherwise
         (error "Unexpected MUC discovery status: ~a" status))))))

(defun muc-service-result-summary (result)
  (with-output-to-string (out nil :element-type 'character)
    (format out "account_domain: ~a~%" (getf result :account-domain))
    (format out "service: ~a~%" (getf result :service-jid))
    (format out "source: ~(~a~)~%" (getf result :source))
    (let ((candidates (getf result :candidates)))
      (when candidates
        (format out "candidates:~%")
        (dolist (candidate candidates)
          (format out "  - ~a" (or (getf candidate :jid) "<unknown>"))
          (when (getf candidate :name)
            (format out " (~a)" (getf candidate :name)))
          (when (muc-service-candidate-p candidate)
            (format out " [muc]"))
          (when (getf candidate :error)
            (format out " error=~a" (getf candidate :error)))
          (terpri out))))))
