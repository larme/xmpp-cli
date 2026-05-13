(in-package #:xmpp-cli/util)

(defvar *data-directory* nil
  "Internal test hook. When non-NIL, use this directory for xmpp-cli state.")

(defun environment-data-directory ()
  (let ((value (uiop:getenv "XMPP_CLI_DATA_DIR")))
    (and value
         (plusp (length value))
         (uiop:ensure-directory-pathname value))))

(defun home-xmpp-cli-directory ()
  (or *data-directory*
      (environment-data-directory)
      (merge-pathnames #P".local/xmpp-cli/" (user-homedir-pathname))))

(defun chmod-best-effort (pathname mode)
  #+(or unix linux darwin)
  (ignore-errors
    (when (probe-file #P"/bin/chmod")
      (uiop:run-program (list "/bin/chmod" mode (namestring (truename pathname)))
                        :ignore-error-status t
                        :output nil
                        :error-output nil)))
  #-(or unix linux darwin)
  (declare (ignore pathname mode))
  pathname)

(defun ensure-private-directory (&optional (directory (home-xmpp-cli-directory)))
  (ensure-directories-exist directory)
  (chmod-best-effort directory "700")
  (let ((logs-directory (merge-pathnames "logs/" directory)))
    (ensure-directories-exist logs-directory)
    (chmod-best-effort logs-directory "700"))
  directory)

(defun write-private-file (pathname contents &key (external-format :utf-8))
  (ensure-directories-exist pathname)
  (with-open-file (out pathname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type 'character
                       :external-format external-format)
    (write-string contents out))
  (chmod-best-effort pathname "600")
  pathname)

(defun read-file-as-string (pathname)
  (with-open-file (in pathname
                      :direction :input
                      :element-type 'character
                      :external-format :utf-8)
    (read-stream-as-string in)))

(defun read-stream-as-string (stream)
  (with-output-to-string (out nil :element-type 'character)
    (loop for ch = (read-char stream nil nil)
          while ch
          do (write-char ch out))))

(defun trim-trailing-slashes (string)
  (let ((end (length string)))
    (loop while (and (> end 1)
                     (char= (char string (1- end)) #\/))
          do (decf end))
    (subseq string 0 end)))

(defun display-path (path)
  (let* ((text (if (pathnamep path) (namestring path) (or path "")))
         (home (trim-trailing-slashes (namestring (user-homedir-pathname))))
         (prefix (concatenate 'string home "/")))
    (cond
      ((string= text home) "~")
      ((and (<= (length prefix) (length text))
            (string= prefix text :end2 (length prefix)))
       (concatenate 'string "~/" (subseq text (length prefix))))
      (t text))))

(defun split-jid (jid)
  "Return USER and DOMAIN from a bare JID user@domain. Signal an error otherwise."
  (unless (and (stringp jid) (plusp (length jid)))
    (error "JID must be a non-empty string."))
  (let ((first-at (position #\@ jid))
        (last-at (position #\@ jid :from-end t)))
    (unless (and first-at last-at (= first-at last-at))
      (error "JID must contain exactly one @: ~a" jid))
    (when (or (zerop first-at) (= first-at (1- (length jid))))
      (error "JID localpart and domain must be non-empty: ~a" jid))
    (when (position #\/ jid)
      (error "Login JID must be bare user@domain, not a resource JID: ~a" jid))
    (values (subseq jid 0 first-at)
            (subseq jid (1+ first-at)))))

(defun now-iso8601 (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year weekday daylight-p timezone)
      (decode-universal-time universal-time)
    (declare (ignore weekday daylight-p))
    (let* ((offset-minutes (round (* -60 timezone)))
           (sign (if (minusp offset-minutes) #\- #\+))
           (abs-offset (abs offset-minutes))
           (offset-hours (floor abs-offset 60))
           (offset-rest-minutes (mod abs-offset 60)))
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~c~2,'0d:~2,'0d"
              year month day hour minute second sign offset-hours offset-rest-minutes))))

(defun parse-fixed-integer (string start end)
  (parse-integer string :start start :end end :junk-allowed nil))

(defun parse-iso8601-timezone (timestamp position)
  (let ((marker (and (< position (length timestamp))
                     (char timestamp position))))
    (cond
      ((null marker)
       nil)
      ((char= marker #\Z)
       0)
      ((or (char= marker #\+) (char= marker #\-))
       (let* ((hours (parse-fixed-integer timestamp
                                          (1+ position)
                                          (+ position 3)))
              (minutes (parse-fixed-integer timestamp
                                            (+ position 4)
                                            (+ position 6)))
              (offset (+ hours (/ minutes 60))))
         ;; ENCODE-UNIVERSAL-TIME expects hours west of GMT. ISO-8601
         ;; offsets use the opposite sign for locations east of GMT.
         (if (char= marker #\+)
             (- offset)
             offset)))
      (t
       nil))))

(defun parse-iso8601 (timestamp)
  (when (and (stringp timestamp)
             (>= (length timestamp) 19)
             (char= (char timestamp 4) #\-)
             (char= (char timestamp 7) #\-)
             (char= (char timestamp 10) #\T)
             (char= (char timestamp 13) #\:)
             (char= (char timestamp 16) #\:))
    (handler-case
        (let ((year (parse-fixed-integer timestamp 0 4))
              (month (parse-fixed-integer timestamp 5 7))
              (day (parse-fixed-integer timestamp 8 10))
              (hour (parse-fixed-integer timestamp 11 13))
              (minute (parse-fixed-integer timestamp 14 16))
              (second (parse-fixed-integer timestamp 17 19))
              (timezone (parse-iso8601-timezone timestamp 19)))
          (if timezone
              (encode-universal-time second minute hour day month year timezone)
              (encode-universal-time second minute hour day month year)))
      (error ()
        nil))))

(defun current-process-id ()
  #+sbcl
  (or (ignore-errors (sb-posix:getpid)) nil)
  #-sbcl
  (let* ((package (find-package "SYSTEM"))
         (symbol (and package (find-symbol "GETPID" package))))
    (and symbol
         (fboundp symbol)
         (ignore-errors (funcall symbol)))))

#+linux
(defun linux-process-state (pid)
  (handler-case
      (with-open-file (in (format nil "/proc/~d/stat" pid)
                          :direction :input
                          :element-type 'character)
        (let* ((line (read-line in nil ""))
               (close (position #\) line :from-end t))
               (state-index (and close (+ close 2))))
          (and state-index
               (< state-index (length line))
               (char line state-index))))
    (error ()
      nil)))

(defun process-exists-p (pid)
  (and (integerp pid)
       (plusp pid)
       #+linux
       (let ((state (linux-process-state pid)))
         (and state
              (not (char= state #\Z))))
       #-linux
       t))

(defun utf-8-octet-length-for-code (code)
  (cond
    ((<= code #x7f) 1)
    ((<= code #x7ff) 2)
    ((<= code #xffff) 3)
    ((<= code #x10ffff) 4)
    (t (error "Character code is outside Unicode range: ~d" code))))

(defun utf-8-byte-length (string)
  (loop for ch across string
        sum (utf-8-octet-length-for-code (char-code ch))))

(defun utf-8-octets (string)
  (let* ((length (utf-8-byte-length string))
         (octets (make-array length :element-type '(unsigned-byte 8)))
         (index 0))
    (labels ((emit (byte)
               (setf (aref octets index) byte)
               (incf index)))
      (loop for ch across string
            for code = (char-code ch)
            do (cond
                 ((<= code #x7f)
                  (emit code))
                 ((<= code #x7ff)
                  (emit (logior #xc0 (ldb (byte 5 6) code)))
                  (emit (logior #x80 (ldb (byte 6 0) code))))
                 ((<= code #xffff)
                  (emit (logior #xe0 (ldb (byte 4 12) code)))
                  (emit (logior #x80 (ldb (byte 6 6) code)))
                  (emit (logior #x80 (ldb (byte 6 0) code))))
                 ((<= code #x10ffff)
                  (emit (logior #xf0 (ldb (byte 3 18) code)))
                  (emit (logior #x80 (ldb (byte 6 12) code)))
                  (emit (logior #x80 (ldb (byte 6 6) code)))
                  (emit (logior #x80 (ldb (byte 6 0) code))))
                 (t
                  (error "Character code is outside Unicode range: ~d" code))))
      octets)))

(defun sha256-hex (string)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :sha256 (utf-8-octets string)))))
