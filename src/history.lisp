(in-package #:xmpp-cli/history)

(defun history-pathname ()
  (merge-pathnames "history.yaml" (home-xmpp-cli-directory)))

(defun history-value-to-yaml (key value)
  (declare (ignore key))
  (if (keywordp value)
      (string-downcase (symbol-name value))
      value))

(defun yaml-value-to-history-value (key value)
  (cond
    ((member key '("kind" "result") :test #'string=)
     (intern (string-upcase value) :keyword))
    ((yaml-null-p value)
     nil)
    (t value)))

(defun history-entry-to-yaml (entry)
  (plist-to-yaml entry :value-to-yaml #'history-value-to-yaml))

(defun yaml-to-history-entry (mapping)
  (unless (listp mapping)
    (error "Malformed history.yaml: history entry must be a mapping."))
  (yaml-to-plist mapping :value-from-yaml #'yaml-value-to-history-value))

(defun history-to-yaml (history)
  (list (cons "entries" (mapcar #'history-entry-to-yaml history))))

(defun yaml-to-history (yaml)
  (unless (listp yaml)
    (error "Malformed history.yaml: expected a mapping."))
  (let ((entries (yaml-value yaml "entries" nil)))
    (unless (listp entries)
      (error "Malformed history.yaml: entries must be a list."))
    (mapcar #'yaml-to-history-entry entries)))

(defun read-history-file (pathname)
  (yaml-to-history (read-yaml-file pathname)))

(defun load-history ()
  (let ((pathname (history-pathname)))
    (if (probe-file pathname)
        (read-history-file pathname)
        nil)))

(defun save-history (history)
  (ensure-private-directory)
  (write-yaml-atomically (history-pathname) (history-to-yaml history)))

(defun append-history (&key profile to kind body bytes sha256 result error)
  (declare (ignore body))
  (let* ((entry (append (list :time (now-iso8601)
                              :profile profile
                              :to to
                              :kind kind
                              :bytes (or bytes 0)
                              :sha256 (or sha256 "")
                              :result result)
                        (when error
                          (list :error error))))
         (history (append (load-history) (list entry))))
    (save-history history)
    entry))
