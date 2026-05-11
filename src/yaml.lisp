(in-package #:xmpp-cli/yaml)

(defconstant +yaml-null+ :yaml-null)

(defstruct yaml-line
  indent
  text)

(defun yaml-null ()
  +yaml-null+)

(defun yaml-null-p (value)
  (eq value +yaml-null+))

(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun starts-with-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun count-leading-spaces (string)
  (loop for index below (length string)
        while (char= (char string index) #\Space)
        finally (return index)))

(defun string-lines (string)
  (with-input-from-string (in string)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun significant-lines (string)
  (let ((result nil))
    (dolist (raw-line (string-lines string) (nreverse result))
      (let* ((line (string-right-trim '(#\Space #\Tab #\Return) raw-line))
             (trimmed (trim-whitespace line)))
        (unless (or (zerop (length trimmed))
                    (char= (char trimmed 0) #\#))
          (when (find #\Tab line)
            (error "Tabs are not supported in YAML indentation: ~a" raw-line))
          (push (make-yaml-line :indent (count-leading-spaces line)
                                :text (subseq line (count-leading-spaces line)))
                result))))))

(defun split-key-value (text)
  (let ((position (position #\: text)))
    (unless position
      (error "Malformed YAML mapping line: ~a" text))
    (let ((key (trim-whitespace (subseq text 0 position)))
          (value (trim-whitespace (subseq text (1+ position)))))
      (unless (plusp (length key))
        (error "YAML mapping key must be non-empty: ~a" text))
      (values key value))))

(defun parse-integer-scalar (text)
  (handler-case
      (multiple-value-bind (value position)
          (parse-integer text :junk-allowed t)
        (and value
             (= position (length text))
             value))
    (error () nil)))

(defun parse-double-quoted-scalar (text)
  (unless (and (>= (length text) 2)
               (char= (char text 0) #\")
               (char= (char text (1- (length text))) #\"))
    (error "Malformed YAML double-quoted scalar: ~a" text))
  (with-output-to-string (out)
    (let ((escaped nil))
      (loop for index from 1 below (1- (length text))
            for char = (char text index)
            do (cond
                 (escaped
                  (write-char
                   (case char
                     (#\n #\Newline)
                     (#\r #\Return)
                     (#\t #\Tab)
                     (#\" #\")
                     (#\\ #\\)
                     (t char))
                   out)
                  (setf escaped nil))
                 ((char= char #\\)
                  (setf escaped t))
                 (t
                  (write-char char out))))
      (when escaped
        (error "Malformed YAML double-quoted scalar: trailing escape in ~a"
               text)))))

(defun parse-single-quoted-scalar (text)
  (unless (and (>= (length text) 2)
               (char= (char text 0) #\')
               (char= (char text (1- (length text))) #\'))
    (error "Malformed YAML single-quoted scalar: ~a" text))
  (with-output-to-string (out)
    (let ((index 1))
      (loop while (< index (1- (length text)))
            for char = (char text index)
            do (cond
                 ((and (char= char #\')
                       (< (1+ index) (1- (length text)))
                       (char= (char text (1+ index)) #\'))
                  (write-char #\' out)
                  (incf index 2))
                 (t
                  (write-char char out)
                  (incf index)))))))

(defun split-inline-sequence (text)
  (let ((inner (string-trim '(#\Space #\Tab)
                            (subseq text 1 (1- (length text)))))
        (items nil)
        (start 0)
        (quote nil)
        (escaped nil))
    (labels ((push-item (end)
               (let ((item (trim-whitespace (subseq inner start end))))
                 (when (plusp (length item))
                   (push item items)))))
      (loop for index below (length inner)
            for char = (char inner index)
            do (cond
                 (escaped
                  (setf escaped nil))
                 ((and quote (char= char #\\) (char= quote #\"))
                  (setf escaped t))
                 (quote
                  (when (char= char quote)
                    (setf quote nil)))
                 ((member char '(#\" #\') :test #'char=)
                  (setf quote char))
                 ((char= char #\,)
                  (push-item index)
                  (setf start (1+ index)))))
      (push-item (length inner)))
    (nreverse items)))

(defun parse-scalar (text)
  (let* ((trimmed (trim-whitespace text))
         (lower (string-downcase trimmed)))
    (cond
      ((string= trimmed "") "")
      ((string= trimmed "[]") nil)
      ((and (>= (length trimmed) 2)
            (char= (char trimmed 0) #\[)
            (char= (char trimmed (1- (length trimmed))) #\]))
       (mapcar #'parse-scalar (split-inline-sequence trimmed)))
      ((and (>= (length trimmed) 2)
            (char= (char trimmed 0) #\"))
       (parse-double-quoted-scalar trimmed))
      ((and (>= (length trimmed) 2)
            (char= (char trimmed 0) #\'))
       (parse-single-quoted-scalar trimmed))
      ((string= lower "null") +yaml-null+)
      ((string= lower "true") t)
      ((string= lower "false") nil)
      ((parse-integer-scalar trimmed))
      (t trimmed))))

(defun mapping-entry-text-p (text)
  (let ((position (position #\: text)))
    (and position
         (plusp position)
         (every (lambda (char)
                  (not (whitespace-char-p char)))
                (subseq text 0 position)))))

(defun parse-yaml (string)
  (let* ((lines (coerce (significant-lines string) 'vector))
         (index 0))
    (labels ((done-p ()
               (>= index (length lines)))
             (current ()
               (unless (done-p)
                 (aref lines index)))
             (parse-node (indent)
               (let ((line (current)))
                 (cond
                   ((null line) nil)
                   ((< (yaml-line-indent line) indent) nil)
                   ((> (yaml-line-indent line) indent)
                    (error "Unexpected YAML indentation before: ~a"
                           (yaml-line-text line)))
                   ((starts-with-p "- " (yaml-line-text line))
                    (parse-sequence indent))
                   (t
                    (parse-mapping indent)))))
             (parse-mapping (indent)
               (let ((pairs nil))
                 (loop until (done-p)
                       for line = (current)
                       for line-indent = (yaml-line-indent line)
                       do (cond
                            ((< line-indent indent)
                             (return))
                            ((> line-indent indent)
                             (error "Unexpected YAML indentation before: ~a"
                                    (yaml-line-text line)))
                            ((starts-with-p "- " (yaml-line-text line))
                             (return))
                            (t
                             (multiple-value-bind (key value-text)
                                 (split-key-value (yaml-line-text line))
                               (incf index)
                               (let ((value
                                       (if (plusp (length value-text))
                                           (parse-scalar value-text)
                                           (let ((next (current)))
                                             (if (and next
                                                      (> (yaml-line-indent next)
                                                         indent))
                                                 (parse-node
                                                  (yaml-line-indent next))
                                                 +yaml-null+)))))
                                 (push (cons key value) pairs))))))
                 (nreverse pairs)))
             (parse-sequence (indent)
               (let ((items nil))
                 (loop until (done-p)
                       for line = (current)
                       for line-indent = (yaml-line-indent line)
                       do (cond
                            ((< line-indent indent)
                             (return))
                            ((> line-indent indent)
                             (error "Unexpected YAML indentation before: ~a"
                                    (yaml-line-text line)))
                            ((not (starts-with-p "- " (yaml-line-text line)))
                             (return))
                            (t
                             (let ((item-text (trim-whitespace
                                               (subseq (yaml-line-text line) 2))))
                               (incf index)
                               (push
                                (cond
                                  ((zerop (length item-text))
                                   (let ((next (current)))
                                     (if (and next
                                              (> (yaml-line-indent next)
                                                 indent))
                                         (parse-node (yaml-line-indent next))
                                         +yaml-null+)))
                                  ((mapping-entry-text-p item-text)
                                   (multiple-value-bind (key value-text)
                                       (split-key-value item-text)
                                     (let ((pairs
                                             (list
                                              (cons key
                                                    (if (plusp
                                                         (length value-text))
                                                        (parse-scalar value-text)
                                                        +yaml-null+)))))
                                       (let ((next (current)))
                                         (when (and next
                                                    (> (yaml-line-indent next)
                                                       indent))
                                           (setf pairs
                                                 (append
                                                  pairs
                                                  (parse-mapping
                                                   (yaml-line-indent next))))))
                                       pairs)))
                                  (t
                                   (parse-scalar item-text)))
                                items)))))
                 (nreverse items))))
      (if (zerop (length lines))
          nil
          (prog1 (parse-node (yaml-line-indent (aref lines 0)))
            (unless (done-p)
              (error "Trailing YAML content at: ~a"
                     (yaml-line-text (current)))))))))

(defun yaml-alist-p (value)
  (and (consp value)
       (every (lambda (entry)
                (and (consp entry)
                     (stringp (car entry))))
              value)))

(defun yaml-sequence-p (value)
  (and (listp value)
       (not (yaml-alist-p value))))

(defun escape-yaml-string (string)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across string
          do (case char
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char char out))))
    (write-char #\" out)))

(defun emit-scalar (value)
  (cond
    ((yaml-null-p value) "null")
    ((stringp value) (escape-yaml-string value))
    ((integerp value) (princ-to-string value))
    ((eq value t) "true")
    ((keywordp value) (escape-yaml-string (string-downcase (symbol-name value))))
    ((null value) "[]")
    (t (escape-yaml-string (princ-to-string value)))))

(defun emit-indent (stream indent)
  (dotimes (i indent)
    (declare (ignore i))
    (write-char #\Space stream)))

(defun emit-mapping-pair (stream key value indent)
  (emit-indent stream indent)
  (write-string key stream)
  (cond
    ((yaml-alist-p value)
     (write-string ":" stream)
     (terpri stream)
     (emit-mapping stream value (+ indent 2)))
    ((yaml-sequence-p value)
     (if value
         (progn
           (write-string ":" stream)
           (terpri stream)
           (emit-sequence stream value (+ indent 2)))
         (format stream ": []~%")))
    (t
     (format stream ": ~a~%" (emit-scalar value)))))

(defun emit-mapping (stream mapping indent)
  (dolist (entry mapping)
    (emit-mapping-pair stream (car entry) (cdr entry) indent)))

(defun emit-sequence (stream sequence indent)
  (dolist (item sequence)
    (cond
      ((yaml-alist-p item)
       (emit-indent stream indent)
       (write-string "- " stream)
       (if item
           (let ((first (first item))
                 (rest (rest item)))
             (write-string (car first) stream)
             (let ((value (cdr first)))
               (cond
                 ((or (yaml-alist-p value) (yaml-sequence-p value))
                  (write-string ":" stream)
                  (terpri stream)
                  (if (yaml-alist-p value)
                      (emit-mapping stream value (+ indent 4))
                      (emit-sequence stream value (+ indent 4))))
                 (t
                  (format stream ": ~a~%" (emit-scalar value)))))
           (dolist (entry rest)
             (emit-mapping-pair stream (car entry) (cdr entry) (+ indent 2))))
           (format stream "{}~%")))
      ((yaml-sequence-p item)
       (emit-indent stream indent)
       (write-string "-" stream)
       (terpri stream)
       (emit-sequence stream item (+ indent 2)))
      (t
       (emit-indent stream indent)
       (format stream "- ~a~%" (emit-scalar item))))))

(defun emit-yaml (value)
  (with-output-to-string (out)
    (cond
      ((yaml-alist-p value)
       (emit-mapping out value 0))
      ((yaml-sequence-p value)
       (emit-sequence out value 0))
      (t
       (format out "~a~%" (emit-scalar value))))))

(defun read-yaml-file (pathname)
  (parse-yaml (xmpp-cli/util:read-file-as-string pathname)))

(defun write-yaml-file (pathname value)
  (write-private-file pathname (emit-yaml value)))

(defun yaml-value (mapping key &optional default)
  (let ((entry (assoc key mapping :test #'string=)))
    (if entry
        (let ((value (cdr entry)))
          (if (yaml-null-p value) default value))
        default)))
