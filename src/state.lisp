(in-package #:xmpp-cli/state)

(defparameter *default-profile-name* "default")

(defun empty-config ()
  (list :default-profile *default-profile-name*
        :profiles nil))

(defun config-pathname ()
  (merge-pathnames "config.sexp" (home-xmpp-cli-directory)))

(defun config-temp-pathname ()
  (merge-pathnames "config.sexp.tmp" (home-xmpp-cli-directory)))

(defun proper-plist-p (plist)
  (and (listp plist) (evenp (length plist))))

(defun validate-profile-entry (entry)
  (unless (and (consp entry)
               (stringp (first entry))
               (proper-plist-p (rest entry)))
    (error "Malformed profile entry in config.sexp: ~s" entry))
  entry)

(defun validate-config (config)
  (unless (proper-plist-p config)
    (error "Malformed config.sexp: expected a property list, got ~s" config))
  (let ((default (getf config :default-profile *default-profile-name*))
        (profiles (getf config :profiles nil)))
    (unless (stringp default)
      (error "Malformed config.sexp: :default-profile must be a string."))
    (unless (listp profiles)
      (error "Malformed config.sexp: :profiles must be a list."))
    (mapc #'validate-profile-entry profiles)
    config))

(defun read-config-file (pathname)
  (with-open-file (in pathname
                      :direction :input
                      :element-type 'character
                      :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (eof (list :eof)))
        (let ((object (read in nil eof)))
          (when (eq object eof)
            (error "Malformed config.sexp: empty file."))
          (validate-config object))))))

(defun load-config ()
  (let ((pathname (config-pathname)))
    (if (probe-file pathname)
        (read-config-file pathname)
        (empty-config))))

(defun print-sexpression (object)
  (with-output-to-string (out)
    (write object
           :stream out
           :case :downcase
           :circle nil
           :pretty nil
           :readably t)
    (terpri out)))

(defun save-config (config)
  (validate-config config)
  (ensure-private-directory)
  (let ((temp (config-temp-pathname))
        (target (config-pathname)))
    (write-private-file temp (print-sexpression config))
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
