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
