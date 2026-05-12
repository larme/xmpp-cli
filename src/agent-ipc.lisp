(in-package #:xmpp-cli/agent-ipc)

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
        (cons "profile" (getf control :profile))))

(defun yaml-to-control (yaml)
  (unless (listp yaml)
    (error "Malformed agent/control.yaml: expected a mapping."))
  (let ((control (list :pid (yaml-value yaml "pid" nil)
                       :host (yaml-value yaml "host" nil)
                       :port (yaml-value yaml "port" nil)
                       :token (yaml-value yaml "token" nil)
                       :started-at (yaml-value yaml "started_at" nil)
                       :profile (yaml-value yaml "profile" nil))))
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

(defun make-ipc-stream (socket)
  (flexi-streams:make-flexi-stream (usocket:socket-stream socket)
                                   :external-format :utf-8))

(defun write-ipc-message (stream message)
  (let ((*print-circle* nil)
        (*print-readably* t))
    (write message :stream stream :readably t)
    (terpri stream)
    (finish-output stream)))

(defun read-ipc-message (stream)
  (let ((*read-eval* nil))
    (read stream nil nil)))

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
                                              :timeout 1)))
          (unwind-protect
               (let ((stream (make-ipc-stream socket)))
                 (write-ipc-message stream
                                    (append (list :token token) payload))
                 (let ((response (read-ipc-message stream)))
                   (values response nil)))
            (ignore-errors
              (usocket:socket-close socket)))))
    (error (condition)
      (values nil condition))))

(defun daemon-response-error (response fallback)
  (or (and (listp response) (getf response :error))
      fallback))

(defun daemon-send (to body)
  (multiple-value-bind (response error) (request-control
                                         (list :op :send
                                               :to to
                                               :body body))
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
