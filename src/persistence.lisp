(in-package #:xmpp-cli/persistence)

(defun proper-plist-p (plist)
  (and (listp plist) (evenp (length plist))))

(defun keyword-to-yaml-key (keyword)
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun yaml-key-to-keyword (key)
  (intern (string-upcase (substitute #\- #\_ key)) :keyword))

(defun plist-to-yaml (plist &key
                              (value-to-yaml
                               (lambda (key value)
                                 (declare (ignore key))
                                 value)))
  (unless (proper-plist-p plist)
    (error "Expected a property list, got ~s." plist))
  (loop for (key value) on plist by #'cddr
        collect (cons (keyword-to-yaml-key key)
                      (funcall value-to-yaml key value))))

(defun yaml-to-plist (mapping &key
                                (value-from-yaml
                                 (lambda (key value)
                                   (declare (ignore key))
                                   value)))
  (unless (listp mapping)
    (error "Expected a YAML mapping, got ~s." mapping))
  (loop for (key . value) in mapping
        append (list (yaml-key-to-keyword key)
                     (funcall value-from-yaml key value))))

(defun yaml-null-to-nil (key value)
  (declare (ignore key))
  (if (yaml-null-p value) nil value))

(defun nil-to-yaml-null (key value)
  (declare (ignore key))
  (if value value (yaml-null)))

(defun yaml-record-p (value)
  (and (listp value)
       (every (lambda (entry)
                (and (consp entry)
                     (stringp (car entry))))
              value)))

(defun read-yaml-record-list-file (pathname item-from-yaml
                                   &key (label (namestring pathname)))
  (if (probe-file pathname)
      (let ((yaml (read-yaml-file pathname)))
        (unless (and (listp yaml)
                     (every #'yaml-record-p yaml))
          (error "Malformed ~a: expected a top-level list of mappings." label))
        (mapcar item-from-yaml yaml))
      nil))

(defun write-yaml-record-list-file (pathname items item-to-yaml)
  (write-yaml-atomically pathname (mapcar item-to-yaml items)))

(defun temporary-sibling-pathname (target)
  (merge-pathnames
   (format nil "~a.~36r.~36r.tmp"
           (file-namestring target)
           (get-universal-time)
           (random 1000000000))
   (uiop:pathname-directory-pathname target)))

(defun write-yaml-atomically (target value)
  (let ((temp (temporary-sibling-pathname target)))
    (unwind-protect
         (progn
           (write-yaml-file temp value)
           (uiop:rename-file-overwriting-target temp target)
           target)
      (when (probe-file temp)
        (ignore-errors
          (delete-file temp))))))
