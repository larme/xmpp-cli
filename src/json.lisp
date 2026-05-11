(in-package #:xmpp-cli/json)

(defconstant +json-null+ :json-null)

(defun json-null ()
  +json-null+)

(defun json-null-p (value)
  (eq value +json-null+))

(defun jzon-null-p (value)
  (eq value 'cl:null))

(defun from-jzon (value)
  (cond
    ((jzon-null-p value)
     +json-null+)
    ((stringp value)
     value)
    ((hash-table-p value)
     (let ((pairs nil))
       (maphash (lambda (key mapped-value)
                  (push (cons key (from-jzon mapped-value)) pairs))
                value)
       (nreverse pairs)))
    ((vectorp value)
     (loop for item across value
           collect (from-jzon item)))
    (t
     value)))

(defun parse-json (text)
  (from-jzon (com.inuoe.jzon:parse text :key-fn nil)))

(defun json-value (object key &optional default)
  (let ((entry (and (listp object)
                    (assoc key object :test #'string=))))
    (if entry
        (let ((value (cdr entry)))
          (if (json-null-p value) default value))
        default)))

(defun json-object-alist-p (value)
  (and (listp value)
       (every (lambda (entry)
                (and (consp entry) (stringp (car entry))))
              value)))

(defun alist-to-hash-table (alist)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry alist table)
      (setf (gethash (car entry) table)
            (to-jzon (cdr entry))))))

(defun list-to-vector (list)
  (coerce (mapcar #'to-jzon list) 'vector))

(defun vector-map (function vector)
  (let ((mapped (make-array (length vector))))
    (loop for index below (length vector)
          do (setf (aref mapped index)
                   (funcall function (aref vector index))))
    mapped))

(defun to-jzon (value)
  (cond
    ((json-null-p value) 'cl:null)
    ((or (stringp value)
         (numberp value)
         (eq value t)
         (null value))
     value)
    ((json-object-alist-p value)
     (alist-to-hash-table value))
    ((listp value)
     (list-to-vector value))
    ((vectorp value)
     (vector-map #'to-jzon value))
    (t
     (princ-to-string value))))

(defun json-compact-string (value)
  (com.inuoe.jzon:stringify (to-jzon value)))
