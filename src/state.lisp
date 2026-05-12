(in-package #:xmpp-cli/state)

(defparameter *default-profile-name* "default")

(defun empty-config ()
  (list :default-profile *default-profile-name*
        :profiles nil))

(defun config-pathname ()
  (merge-pathnames "config.yaml" (home-xmpp-cli-directory)))

(defun validate-profile-entry (entry)
  (unless (and (consp entry)
               (stringp (first entry))
               (proper-plist-p (rest entry)))
    (error "Malformed profile entry in config.yaml: ~s" entry))
  entry)

(defun validate-config (config)
  (unless (proper-plist-p config)
    (error "Malformed config.yaml: expected a property list, got ~s" config))
  (let ((default (getf config :default-profile *default-profile-name*))
        (profiles (getf config :profiles nil)))
    (unless (stringp default)
      (error "Malformed config.yaml: default_profile must be a string."))
    (unless (listp profiles)
      (error "Malformed config.yaml: profiles must be a list."))
    (mapc #'validate-profile-entry profiles)
    config))

(defun profile-value-to-yaml (key value)
  (declare (ignore key))
  (if (keywordp value)
      (string-downcase (symbol-name value))
      value))

(defun yaml-value-to-profile-value (key value)
  (cond
    ((string= key "mechanism")
     (intern (string-upcase value) :keyword))
    ((yaml-null-p value)
     nil)
    (t value)))

(defun profile-to-yaml (entry)
  (cons (cons "name" (first entry))
        (plist-to-yaml (rest entry)
                       :value-to-yaml #'profile-value-to-yaml)))

(defun yaml-to-profile-entry (mapping)
  (unless (listp mapping)
    (error "Malformed config.yaml: profile entry must be a mapping."))
  (let ((name (yaml-value mapping "name")))
    (unless (and (stringp name) (plusp (length name)))
      (error "Malformed config.yaml: profile name must be a non-empty string."))
    (cons name
          (yaml-to-plist (remove "name" mapping :key #'car :test #'string=)
                         :value-from-yaml #'yaml-value-to-profile-value))))

(defun config-to-yaml (config)
  (validate-config config)
  (list (cons "default_profile" (default-profile-name config))
        (cons "profiles" (mapcar #'profile-to-yaml
                                  (getf config :profiles)))))

(defun yaml-to-config (yaml)
  (unless (listp yaml)
    (error "Malformed config.yaml: expected a mapping."))
  (validate-config
   (list :default-profile (yaml-value yaml
                                      "default_profile"
                                      *default-profile-name*)
         :profiles (mapcar #'yaml-to-profile-entry
                           (yaml-value yaml "profiles" nil)))))

(defun read-config-file (pathname)
  (yaml-to-config (read-yaml-file pathname)))

(defun load-config ()
  (let ((pathname (config-pathname)))
    (if (probe-file pathname)
        (read-config-file pathname)
        (empty-config))))

(defun save-config (config)
  (validate-config config)
  (ensure-private-directory)
  (write-yaml-atomically (config-pathname) (config-to-yaml config)))

(defun default-profile-name (config)
  (or (getf config :default-profile) *default-profile-name*))

(defun profile (config name)
  (let ((entry (assoc name (getf config :profiles) :test #'string=)))
    (and entry (copy-list (rest entry)))))

(defun validate-profile-plist (profile-plist)
  (unless (proper-plist-p profile-plist)
    (error "Profile must be a property list: ~s" profile-plist))
  (let ((jid (getf profile-plist :jid)))
    (when jid
      (split-jid jid)))
  profile-plist)

(defun set-profile (config name profile-plist &key make-default)
  (validate-config config)
  (unless (and (stringp name) (plusp (length name)))
    (error "Profile name must be a non-empty string."))
  (validate-profile-plist profile-plist)
  (let* ((profiles (remove name (getf config :profiles) :test #'string= :key #'first))
         (default (if make-default name (default-profile-name config))))
    (list :default-profile default
          :profiles (cons (cons name (copy-list profile-plist)) profiles))))
