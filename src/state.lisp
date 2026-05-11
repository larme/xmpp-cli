(in-package #:xmpp-cli/state)

(defparameter *default-profile-name* "default")

(defun empty-config ()
  (list :default-profile *default-profile-name*
        :profiles nil))

(defun config-pathname ()
  (merge-pathnames "config.yaml" (home-xmpp-cli-directory)))

(defun config-temp-pathname ()
  (merge-pathnames "config.yaml.tmp" (home-xmpp-cli-directory)))

(defun proper-plist-p (plist)
  (and (listp plist) (evenp (length plist))))

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

(defun keyword-to-yaml-key (keyword)
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun yaml-key-to-keyword (key)
  (intern (string-upcase (substitute #\- #\_ key)) :keyword))

(defun profile-value-to-yaml (value)
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
  (let ((plist (rest entry))
        (mapping (list (cons "name" (first entry)))))
    (loop for (key value) on plist by #'cddr
          do (push (cons (keyword-to-yaml-key key)
                         (profile-value-to-yaml value))
                   mapping))
    (nreverse mapping)))

(defun yaml-to-profile-entry (mapping)
  (unless (listp mapping)
    (error "Malformed config.yaml: profile entry must be a mapping."))
  (let ((name (yaml-value mapping "name")))
    (unless (and (stringp name) (plusp (length name)))
      (error "Malformed config.yaml: profile name must be a non-empty string."))
    (cons name
          (loop for (key . value) in mapping
                unless (string= key "name")
                  append (list (yaml-key-to-keyword key)
                               (yaml-value-to-profile-value key value))))))

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
  (let ((temp (config-temp-pathname))
        (target (config-pathname)))
    (write-yaml-file temp (config-to-yaml config))
    (uiop:rename-file-overwriting-target temp target)
    target))

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
