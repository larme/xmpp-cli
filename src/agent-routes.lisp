(in-package #:xmpp-cli/agent-routes)

(defparameter *code-alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun agent-directory ()
  (merge-pathnames "agent/" (home-xmpp-cli-directory)))

(defun routes-pathname ()
  (merge-pathnames "routes.yaml" (agent-directory)))

(defun routes-temp-pathname ()
  (merge-pathnames "routes.yaml.tmp" (agent-directory)))

(defun ensure-agent-directory ()
  (ensure-private-directory (agent-directory)))

(defun canonical-route-identity (&key host
                                      tmux-socket
                                      tmux-session-id
                                      tmux-window-id
                                      tmux-pane-id
                                      agent
                                      agent-session)
  (format nil "host=~a~%tmux-socket=~a~%tmux-session-id=~a~%tmux-window-id=~a~%tmux-pane-id=~a~%agent=~a~%agent-session=~a"
          (or host "")
          (or tmux-socket "")
          (or tmux-session-id "")
          (or tmux-window-id "")
          (or tmux-pane-id "")
          (string-downcase (string (or agent "")))
          (or agent-session "")))

(defun route-id-for-identity (identity)
  (sha256-hex identity))

(defun random-route-code (&optional (length 4))
  (with-output-to-string (out)
    (dotimes (index length)
      (declare (ignore index))
      (write-char (char *code-alphabet* (random (length *code-alphabet*)))
                  out))))

(defun route-code-equal (left right)
  (and left right (string-equal left right)))

(defun yaml-key-to-keyword (key)
  (intern (string-upcase (substitute #\- #\_ key)) :keyword))

(defun keyword-to-yaml-key (keyword)
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun yaml-to-route-value (key value)
  (declare (ignore key))
  (if (yaml-null-p value) nil value))

(defun route-value-to-yaml (key value)
  (declare (ignore key))
  (if value value (yaml-null)))

(defun yaml-to-route (mapping)
  (unless (listp mapping)
    (error "Malformed agent/routes.yaml: route entry must be a mapping."))
  (loop for (key . value) in mapping
        append (list (yaml-key-to-keyword key)
                     (yaml-to-route-value key value))))

(defun route-to-yaml (route)
  (loop for (key value) on route by #'cddr
        collect (cons (keyword-to-yaml-key key)
                      (route-value-to-yaml key value))))

(defun yaml-to-routes-file (yaml)
  (unless (listp yaml)
    (error "Malformed agent/routes.yaml: expected a mapping."))
  (let ((version (yaml-value yaml "version" 1))
        (routes (yaml-value yaml "routes" nil)))
    (unless (= version 1)
      (error "Unsupported agent/routes.yaml version: ~a" version))
    (unless (listp routes)
      (error "Malformed agent/routes.yaml: routes must be a list."))
    (mapcar #'yaml-to-route routes)))

(defun routes-file-to-yaml (routes)
  (list (cons "version" 1)
        (cons "routes" (mapcar #'route-to-yaml routes))))

(defun load-routes ()
  (let ((pathname (routes-pathname)))
    (if (probe-file pathname)
        (yaml-to-routes-file (read-yaml-file pathname))
        nil)))

(defun save-routes (routes)
  (ensure-agent-directory)
  (let ((temp (routes-temp-pathname))
        (target (routes-pathname)))
    (write-yaml-file temp (routes-file-to-yaml routes))
    (uiop:rename-file-overwriting-target temp target)
    target))

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

(defun route-activity-time (route)
  (let ((times (remove nil
                       (mapcar (lambda (key)
                                 (parse-iso8601 (getf route key)))
                               '(:last-used-at :last-seen-at :created-at)))))
    (and times (reduce #'max times))))

(defun last-active-route (&optional (routes (load-routes)))
  (let ((best nil)
        (best-time nil))
    (dolist (route routes best)
      (let ((time (or (route-activity-time route) 0)))
        (when (or (null best) (> time best-time))
          (setf best route
                best-time time))))))

(defun route-expired-p (route ttl-days &optional (now (get-universal-time)))
  (and ttl-days
       (let ((activity-time (route-activity-time route)))
         (or (null activity-time)
             (> (- now activity-time)
                (* ttl-days 24 60 60))))))

(defun active-routes (routes ttl-days &optional (now (get-universal-time)))
  (if ttl-days
      (remove-if (lambda (route)
                   (route-expired-p route ttl-days now))
                 routes)
      routes))

(defun load-active-routes (&key route-ttl-days)
  (let* ((routes (load-routes))
         (active (active-routes routes route-ttl-days)))
    (when (and route-ttl-days
               (/= (length routes) (length active)))
      (save-routes active))
    active))

(defun find-route-by-id (routes route-id)
  (find route-id routes :test #'string= :key (lambda (route)
                                               (getf route :route-id))))

(defun find-route-by-code (code &optional (routes (load-routes)) route-ttl-days)
  (let ((route (find code routes :test #'route-code-equal :key (lambda (entry)
                                                                 (getf entry :code)))))
    (and route
         (not (route-expired-p route route-ttl-days))
         route)))

(defun find-active-route-by-code (code route-ttl-days)
  (find-route-by-code code
                      (load-active-routes :route-ttl-days route-ttl-days)
                      route-ttl-days))

(defun used-code-p (code routes)
  (find-route-by-code code routes))

(defun allocate-route-code (routes code-length)
  (loop repeat 10000
        for code = (random-route-code code-length)
        unless (used-code-p code routes)
          return code
        finally (error "Could not allocate a unique route code after many attempts.")))

(defun update-route-metadata (route metadata now)
  (let ((updated (copy-list route)))
    (loop for (key value) on metadata by #'cddr
          do (setf (getf updated key) value))
    (setf (getf updated :last-seen-at) now)
    (setf (getf updated :notify-count)
          (1+ (or (getf updated :notify-count) 0)))
    updated))

(defun make-route (route-id code identity metadata now)
  (append (list :route-id route-id
                :code code
                :identity identity)
          metadata
          (list :created-at now
                :last-seen-at now
                :last-used-at nil
                :notify-count 1)))

(defun ensure-route (identity &key
                                (code-length 4)
                                route-ttl-days
                                agent
                                agent-session
                                host
                                cwd
                                display-cwd
                                tmux-socket
                                tmux-client-name
                                tmux-client-tty
                                tmux-session-id
                                tmux-window-id
                                tmux-pane-id)
  (let* ((routes (load-active-routes :route-ttl-days route-ttl-days))
         (route-id (route-id-for-identity identity))
         (existing (find-route-by-id routes route-id))
         (now (now-iso8601))
         (metadata (list :agent agent
                         :agent-session agent-session
                         :host host
                         :cwd cwd
                         :display-cwd display-cwd
                         :tmux-socket tmux-socket
                         :tmux-client-name tmux-client-name
                         :tmux-client-tty tmux-client-tty
                         :tmux-session-id tmux-session-id
                         :tmux-window-id tmux-window-id
                         :tmux-pane-id tmux-pane-id)))
    (if existing
        (let* ((updated (update-route-metadata existing metadata now))
               (new-routes (cons updated (remove existing routes :test #'eq))))
          (save-routes new-routes)
          (values updated new-routes nil))
        (let* ((code (allocate-route-code routes code-length))
               (route (make-route route-id code identity metadata now))
               (new-routes (cons route routes)))
          (save-routes new-routes)
          (values route new-routes t)))))

(defun mark-route-used (route &optional (now (now-iso8601)))
  (let* ((routes (load-routes))
         (route-id (getf route :route-id))
         (existing (and route-id (find-route-by-id routes route-id))))
    (unless existing
      (error "Route no longer exists for code ~a." (getf route :code)))
    (let* ((updated (copy-list existing))
           (new-routes nil))
      (setf (getf updated :last-used-at) now)
      (setf new-routes (cons updated (remove existing routes :test #'eq)))
      (save-routes new-routes)
      updated)))
