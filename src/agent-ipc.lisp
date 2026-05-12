(in-package #:xmpp-cli/agent-ipc)

(defparameter *control-connect-timeout-seconds* 1)
(defparameter *control-io-timeout-seconds* 10)
(defparameter *max-ipc-frame-octets* (* 1024 1024))
(defparameter *max-ipc-frame-length-line-chars* 20)

(defun control-pathname ()
  (merge-pathnames "control.yaml" (agent-directory)))

(defun control-temp-pathname ()
  (merge-pathnames "control.yaml.tmp" (agent-directory)))

(defun daemon-lock-pathname ()
  (merge-pathnames "daemon.lock" (agent-directory)))

(defun control-to-yaml (control)
  (list (cons "pid" (getf control :pid))
        (cons "host" (getf control :host))
        (cons "port" (getf control :port))
        (cons "token" (getf control :token))
        (cons "started_at" (getf control :started-at))
        (cons "profile" (getf control :profile))
        (cons "profile_jid" (getf control :profile-jid))
        (cons "profile_digest" (getf control :profile-digest))))

(defun yaml-to-control (yaml)
  (unless (listp yaml)
    (error "Malformed agent/control.yaml: expected a mapping."))
  (let ((control (list :pid (yaml-value yaml "pid" nil)
                       :host (yaml-value yaml "host" nil)
                       :port (yaml-value yaml "port" nil)
                       :token (yaml-value yaml "token" nil)
                       :started-at (yaml-value yaml "started_at" nil)
                       :profile (yaml-value yaml "profile" nil)
                       :profile-jid (yaml-value yaml "profile_jid" nil)
                       :profile-digest (yaml-value yaml "profile_digest" nil))))
    (unless (and (stringp (getf control :host))
                 (plusp (length (getf control :host)))
                 (integerp (getf control :port))
                 (plusp (getf control :port))
                 (stringp (getf control :token))
                 (plusp (length (getf control :token))))
      (error "Malformed agent/control.yaml: missing host, port, or token."))
    control))

(defun lock-to-yaml (token pid)
  (let ((mapping (list (cons "token" token)
                       (cons "created_at" (now-iso8601)))))
    (when pid
      (setf mapping (append mapping (list (cons "pid" pid)))))
    mapping))

(defun yaml-to-lock (yaml)
  (unless (listp yaml)
    (error "Malformed agent/daemon.lock: expected a mapping."))
  (let ((lock (list :token (yaml-value yaml "token" nil)
                    :created-at (yaml-value yaml "created_at" nil)
                    :pid (yaml-value yaml "pid" nil))))
    (unless (and (stringp (getf lock :token))
                 (plusp (length (getf lock :token))))
      (error "Malformed agent/daemon.lock: missing token."))
    (when (and (getf lock :pid)
               (not (integerp (getf lock :pid))))
      (error "Malformed agent/daemon.lock: pid must be an integer."))
    lock))

(defun load-control ()
  (let ((pathname (control-pathname)))
    (and (probe-file pathname)
         (yaml-to-control (read-yaml-file pathname)))))

(defun load-daemon-lock ()
  (let ((pathname (daemon-lock-pathname)))
    (and (probe-file pathname)
         (yaml-to-lock (read-yaml-file pathname)))))

(defun save-control (control)
  (ensure-private-directory (agent-directory))
  (let ((temp (control-temp-pathname))
        (target (control-pathname)))
    (write-yaml-file temp (control-to-yaml control))
    (uiop:rename-file-overwriting-target temp target)
    target))

(defun acquire-daemon-lock (token &key pid)
  (ensure-private-directory (agent-directory))
  (handler-case
      (let ((stream (open (daemon-lock-pathname)
                          :direction :output
                          :if-exists nil
                          :if-does-not-exist :create
                          :element-type 'character
                          :external-format :utf-8)))
        (when stream
          (unwind-protect
               (progn
                 (write-string (emit-yaml (lock-to-yaml token pid)) stream)
                 (finish-output stream)
                 t)
            (close stream))))
    (file-error ()
      nil)))

(defun daemon-lock-owned-p (token)
  (handler-case
      (let ((lock (load-daemon-lock)))
        (and lock
             (string= token (getf lock :token))))
    (error ()
      nil)))

(defun release-daemon-lock (token)
  (when (and token (daemon-lock-owned-p token))
    (ignore-errors
      (delete-file (daemon-lock-pathname)))
    t))

(defun delete-stale-daemon-lock (&optional expected-token)
  (when (or (null expected-token)
            (handler-case
                (let ((lock (load-daemon-lock)))
                  (and lock
                       (string= expected-token (getf lock :token))))
              (error ()
                nil)))
    (ignore-errors
      (delete-file (daemon-lock-pathname)))
    t))

(defun delete-control (&optional expected-token)
  (when (or (null expected-token)
            (handler-case
                (let ((control (load-control)))
                  (and control
                       (string= expected-token (getf control :token))))
              (error ()
                nil)))
    (ignore-errors
      (delete-file (control-pathname)))
    t))

(defun make-control-token ()
  (string-downcase
   (ironclad:byte-array-to-hex-string (ironclad:random-data 32))))

(defun profile-digest (profile)
  (sha256-hex
   (with-output-to-string (out nil :element-type 'character)
     (let ((*print-circle* nil)
           (*print-pretty* nil)
           (*print-readably* t))
       (write profile :stream out :readably t)))))

(defun make-ipc-stream (socket)
  (usocket:socket-stream socket))

(defun ipc-payload-string (message)
  (with-output-to-string (out nil :element-type 'character)
    (let ((*print-circle* nil)
          (*print-readably* t)
          (*print-pretty* nil))
      (write message :stream out :readably t))))

(defun write-ascii-octets (stream string)
  (loop for char across string
        for code = (char-code char)
        do (unless (<= 0 code #x7f)
             (error "Cannot write non-ASCII byte to IPC frame header: ~s."
                    char))
           (write-byte code stream)))

(defun write-ipc-message (stream message)
  (let* ((payload (ipc-payload-string message))
         (octets (utf-8-octets payload)))
    (validate-ipc-frame-length (length octets))
    (write-ascii-octets stream (princ-to-string (length octets)))
    (write-byte 10 stream)
    (write-sequence octets stream)
    (finish-output stream)))

(defun parse-ipc-payload (payload)
  (let ((*read-eval* nil))
    (read-from-string payload nil nil)))

(defun parse-ipc-length (line)
  (unless line
    (error "Daemon IPC connection closed before message length."))
  (when (> (length line) *max-ipc-frame-length-line-chars*)
    (error "Daemon IPC message length is too long."))
  (validate-ipc-frame-length
   (parse-integer line :junk-allowed nil)))

(defun validate-ipc-frame-length (length)
  (unless (and (integerp length)
               (not (minusp length))
               (<= length *max-ipc-frame-octets*))
    (error "Daemon IPC message length ~a exceeds maximum ~d octets."
           length
           *max-ipc-frame-octets*))
  length)

(defun decode-ipc-payload-octets (octets)
  (flexi-streams:octets-to-string octets :external-format :utf-8))

(defun read-ipc-line (stream)
  (let ((count 0))
    (with-output-to-string (out nil :element-type 'character)
      (loop for byte = (read-byte stream nil nil)
            do (unless byte
                 (error "Daemon IPC connection closed before message length."))
               (cond
                 ((= byte 10)
                  (return))
                 ((= byte 13)
                  nil)
                 ((<= 0 byte #x7f)
                  (when (>= count *max-ipc-frame-length-line-chars*)
                    (error "Daemon IPC message length is too long."))
                  (write-char (code-char byte) out)
                  (incf count))
                 (t
                  (error "Daemon IPC message length contains non-ASCII byte: ~d."
                         byte)))))))

(defun read-ipc-payload (stream length)
  (setf length (validate-ipc-frame-length length))
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (loop for index below length
          for byte = (read-byte stream nil nil)
          do (unless byte
               (error "Daemon IPC connection closed before message body."))
             (setf (aref octets index) byte))
    octets))

(defun read-ipc-message (stream)
  (parse-ipc-payload
   (decode-ipc-payload-octets
    (read-ipc-payload stream
                      (parse-ipc-length (read-ipc-line stream))))))

(defun control-deadline (timeout-seconds)
  (+ (get-internal-real-time)
     (round (* timeout-seconds internal-time-units-per-second))))

(defun control-time-remaining (deadline)
  (max 0
       (/ (- deadline (get-internal-real-time))
          internal-time-units-per-second)))

(defun socket-readable-p (socket timeout)
  (multiple-value-bind (ready remaining)
      (usocket:wait-for-input socket :timeout timeout :ready-only t)
    (declare (ignore remaining))
    (and ready t)))

(defun read-byte-with-timeout (socket stream deadline label)
  (let ((remaining (control-time-remaining deadline)))
    (unless (plusp remaining)
      (error "~a timed out." label))
    (unless (socket-readable-p socket remaining)
      (error "~a timed out." label))
    (or (read-byte stream nil nil)
        (error "~a connection closed." label))))

(defun read-ipc-line-with-timeout (socket stream deadline label)
  (let ((count 0))
    (with-output-to-string (out nil :element-type 'character)
      (loop for byte = (read-byte-with-timeout socket stream deadline label)
            do (cond
                 ((= byte 10)
                  (return))
                 ((= byte 13)
                  nil)
                 ((<= 0 byte #x7f)
                  (when (>= count *max-ipc-frame-length-line-chars*)
                    (error "~a length is too long." label))
                  (write-char (code-char byte) out)
                  (incf count))
                 (t
                  (error "~a length contains non-ASCII byte: ~d."
                         label
                         byte)))))))

(defun read-ipc-payload-with-timeout (socket stream length deadline label)
  (setf length (validate-ipc-frame-length length))
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (loop for index below length
          for byte = (read-byte-with-timeout socket stream deadline label)
          do (setf (aref octets index) byte))
    octets))

(defun read-ipc-message-with-timeout (socket stream timeout-seconds label)
  (let* ((deadline (control-deadline timeout-seconds))
         (length (parse-ipc-length
                  (read-ipc-line-with-timeout socket
                                              stream
                                              deadline
                                              label))))
    (parse-ipc-payload
     (decode-ipc-payload-octets
      (read-ipc-payload-with-timeout socket
                                     stream
                                     length
                                     deadline
                                     label)))))

(defun request-control (payload)
  "Send PAYLOAD to the running daemon. Return RESPONSE and ERROR."
  (handler-case
      (let* ((control (load-control))
             (host (getf control :host))
             (port (getf control :port))
             (token (getf control :token)))
        (unless control
          (return-from request-control (values nil :no-control)))
        (let ((socket (usocket:socket-connect host
                                              port
                                              :element-type '(unsigned-byte 8)
                                              :timeout
                                              *control-connect-timeout-seconds*)))
          (unwind-protect
               (let ((stream (make-ipc-stream socket)))
                 (write-ipc-message stream
                                    (append (list :token token) payload))
                 (let ((response (read-ipc-message-with-timeout
                                  socket
                                  stream
                                  *control-io-timeout-seconds*
                                  "Daemon IPC response")))
                   (values response nil)))
            (ignore-errors
              (usocket:socket-close socket)))))
    (error (condition)
      (values nil condition))))

(defun daemon-response-error (response fallback)
  (or (and (listp response) (getf response :error))
      fallback))

(defun daemon-send (to body &key expected-profile-digest)
  (multiple-value-bind (response error) (request-control
                                         (list :op :send
                                               :to to
                                               :body body
                                               :expected-profile-digest
                                               expected-profile-digest))
    (cond
      ((and (listp response) (getf response :ok))
       (values t response nil))
      (response
       (values nil response (daemon-response-error response "daemon send failed")))
      (t
       (values nil nil error)))))

(defun daemon-status ()
  (multiple-value-bind (response error) (request-control (list :op :status))
    (cond
      ((and (listp response) (getf response :ok))
       (values t response nil))
      (response
       (values nil response (daemon-response-error response "daemon status failed")))
      (t
       (values nil nil error)))))

(defun daemon-stop ()
  (multiple-value-bind (response error) (request-control (list :op :stop))
    (cond
      ((and (listp response) (getf response :ok))
       (values t response nil))
      (response
       (values nil response (daemon-response-error response "daemon stop failed")))
      (t
       (values nil nil error)))))
