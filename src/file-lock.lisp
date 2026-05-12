(in-package #:xmpp-cli/file-lock)

(defparameter *file-lock-wait-interval-seconds* 0.05)

(defun file-lock-to-yaml (token pid)
  (let ((mapping (list (cons "token" token)
                       (cons "created_at" (now-iso8601)))))
    (when pid
      (setf mapping (append mapping (list (cons "pid" pid)))))
    mapping))

(defun yaml-to-file-lock (yaml label)
  (unless (listp yaml)
    (error "Malformed ~a: expected a mapping." label))
  (let ((lock (list :token (yaml-value yaml "token" nil)
                    :created-at (yaml-value yaml "created_at" nil)
                    :pid (yaml-value yaml "pid" nil))))
    (unless (and (stringp (getf lock :token))
                 (plusp (length (getf lock :token))))
      (error "Malformed ~a: missing token." label))
    (when (and (getf lock :pid)
               (not (integerp (getf lock :pid))))
      (error "Malformed ~a: pid must be an integer." label))
    lock))

(defun load-file-lock (pathname &key (label "lock file"))
  (and (probe-file pathname)
       (yaml-to-file-lock (read-yaml-file pathname) label)))

(defun try-acquire-file-lock (pathname token &key pid)
  (handler-case
      (let ((stream (open pathname
                          :direction :output
                          :if-exists nil
                          :if-does-not-exist :create
                          :element-type 'character
                          :external-format :utf-8)))
        (when stream
          (unwind-protect
               (progn
                 (write-string (emit-yaml (file-lock-to-yaml token pid))
                               stream)
                 (finish-output stream)
                 t)
            (close stream))))
    (file-error ()
      nil)))

(defun file-lock-owned-p (pathname token &key (label "lock file"))
  (handler-case
      (let ((lock (load-file-lock pathname :label label)))
        (and lock
             (string= token (getf lock :token))))
    (error ()
      nil)))

(defun release-file-lock (pathname token &key (label "lock file"))
  (when (and token (file-lock-owned-p pathname token :label label))
    (ignore-errors
      (delete-file pathname))
    t))

(defun delete-file-lock (pathname &optional expected-token
                           &key (label "lock file"))
  (when (or (null expected-token)
            (file-lock-owned-p pathname expected-token :label label))
    (ignore-errors
      (delete-file pathname))
    t))

(defun file-lock-stale-p (pathname lock &key stale-seconds use-mtime-p)
  (let ((pid (and lock (getf lock :pid))))
    (cond
      (pid
       (not (process-exists-p pid)))
      ((and use-mtime-p stale-seconds)
       (let ((write-date (and (probe-file pathname)
                              (file-write-date pathname))))
         (and write-date
              (> (- (get-universal-time) write-date)
                 stale-seconds))))
      (t
       nil))))

(defun acquire-file-lock (pathname token &key
                                         (pid (current-process-id))
                                         timeout-seconds
                                         stale-seconds
                                         use-mtime-p
                                         (label "lock file"))
  (let ((deadline (and timeout-seconds
                       (+ (get-internal-real-time)
                          (round (* timeout-seconds
                                    internal-time-units-per-second))))))
    (loop
      (when (try-acquire-file-lock pathname token :pid pid)
        (return t))
      (let ((lock (handler-case
                      (load-file-lock pathname :label label)
                    (error ()
                      nil))))
        (when (file-lock-stale-p pathname
                                 lock
                                 :stale-seconds stale-seconds
                                 :use-mtime-p use-mtime-p)
          (ignore-errors
            (delete-file pathname))))
      (when (and deadline
                 (>= (get-internal-real-time) deadline))
        (error "Timed out waiting for ~a at ~a."
               label
               (namestring pathname)))
      (sleep *file-lock-wait-interval-seconds*))))

(defun call-with-file-lock (pathname thunk &key
                                            token
                                            pid
                                            timeout-seconds
                                            stale-seconds
                                            use-mtime-p
                                            (label "lock file"))
  (let ((token (or token
                   (format nil "~36r-~36r-~36r"
                           (get-universal-time)
                           (get-internal-real-time)
                           (random 1000000000)))))
    (unwind-protect
         (progn
           (acquire-file-lock pathname
                              token
                              :pid (or pid (current-process-id))
                              :timeout-seconds timeout-seconds
                              :stale-seconds stale-seconds
                              :use-mtime-p use-mtime-p
                              :label label)
           (funcall thunk))
      (release-file-lock pathname token :label label))))
