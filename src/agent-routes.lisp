(in-package #:xmpp-cli/agent-routes)

(defparameter *code-alphabet* "abcdefghijklmnopqrstuvwxyz")
(defparameter *routes-lock-stale-seconds* 30)
(defparameter *routes-lock-wait-seconds* 10)

(defun routes-pathname ()
  (merge-pathnames "routes.yaml" (agent-directory)))

(defun routes-lock-pathname ()
  (merge-pathnames "routes.lock" (agent-directory)))

(defun call-with-routes-lock (thunk)
  (ensure-agent-directory)
  (call-with-file-lock (routes-lock-pathname)
                       thunk
                       :timeout-seconds *routes-lock-wait-seconds*
                       :stale-seconds *routes-lock-stale-seconds*
                       :use-mtime-p t
                       :label "agent route lock"))

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

(defun yaml-to-route (mapping)
  (unless (listp mapping)
    (error "Malformed agent/routes.yaml: route entry must be a mapping."))
  (yaml-to-plist mapping :value-from-yaml #'yaml-null-to-nil))

(defun route-to-yaml (route)
  (plist-to-yaml route :value-to-yaml #'nil-to-yaml-null))

(defun load-routes-from-disk ()
  (read-yaml-record-list-file (routes-pathname)
                              #'yaml-to-route
                              :label "agent/routes.yaml"))

(defun save-routes-to-disk (routes)
  (ensure-agent-directory)
  (write-yaml-record-list-file (routes-pathname)
                               routes
                               #'route-to-yaml))

(defun load-routes ()
  (load-routes-from-disk))

(defun save-routes (routes)
  (call-with-routes-lock
   (lambda ()
     (save-routes-to-disk routes))))

(defun route-activity-time (route)
  (let ((times (remove nil
                       (mapcar (lambda (key)
                                 (parse-iso8601 (getf route key)))
                               '(:last-used-at :last-seen-at :created-at)))))
    (and times (reduce #'max times))))

(defun route-direct-activity-time (route)
  (let ((times (remove nil
                       (mapcar (lambda (key)
                                 (parse-iso8601 (getf route key)))
                               '(:last-direct-used-at
                                 :last-seen-at
                                 :created-at)))))
    (and times (reduce #'max times))))

(defun last-active-route (&optional (routes (load-routes)))
  (let ((best nil)
        (best-time nil))
    (dolist (route routes best)
      (let ((time (or (route-direct-activity-time route) 0)))
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

(defun load-active-routes-from-disk (&key route-ttl-days)
  (let* ((routes (load-routes-from-disk))
         (active (active-routes routes route-ttl-days)))
    (when (and route-ttl-days
               (/= (length routes) (length active)))
      (save-routes-to-disk active))
    active))

(defun load-active-routes (&key route-ttl-days)
  (call-with-routes-lock
   (lambda ()
     (load-active-routes-from-disk :route-ttl-days route-ttl-days))))

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

(defun route-metadata-value-present-p (value)
  (not (or (null value)
           (and (stringp value)
                (zerop (length value))))))

(defun update-route-metadata (route metadata now)
  (let ((updated (copy-list route)))
    (loop for (key value) on metadata by #'cddr
          do (when (or (route-metadata-value-present-p value)
                       (null (getf updated key)))
               (setf (getf updated key) value)))
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
                :last-direct-used-at nil
                :notify-count 1)))

(defun non-empty-route-string (value)
  (and (stringp value)
       (plusp (length value))
       value))

(defun route-value-compatible-p (left right &key (test #'string=))
  (let ((left (non-empty-route-string left))
        (right (non-empty-route-string right)))
    (or (null left)
        (null right)
        (funcall test left right))))

(defun route-agent-compatible-p (left right)
  (let ((left (string-downcase (string (or left ""))))
        (right (string-downcase (string (or right "")))))
    (or (zerop (length left))
        (zerop (length right))
        (string= left right))))

(defun same-tmux-target-route-p (route metadata)
  (let ((route-pane (non-empty-route-string (getf route :tmux-pane-id)))
        (metadata-pane (non-empty-route-string (getf metadata :tmux-pane-id))))
    (and route-pane
         metadata-pane
         (string= route-pane metadata-pane)
         (route-agent-compatible-p (getf route :agent)
                                   (getf metadata :agent))
         (route-value-compatible-p (getf route :host)
                                   (getf metadata :host)
                                   :test #'string-equal)
         (route-value-compatible-p (getf route :tmux-socket)
                                   (getf metadata :tmux-socket))
         (route-value-compatible-p (getf route :tmux-session-id)
                                   (getf metadata :tmux-session-id))
         (route-value-compatible-p (getf route :tmux-window-id)
                                   (getf metadata :tmux-window-id)))))

(defun find-route-by-tmux-target (routes metadata)
  (find-if (lambda (route)
             (same-tmux-target-route-p route metadata))
           routes))

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
  (call-with-routes-lock
   (lambda ()
     (let* ((routes (load-active-routes-from-disk
                     :route-ttl-days route-ttl-days))
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
       (unless existing
         (setf existing (find-route-by-tmux-target routes metadata)))
       (if existing
           (let* ((updated (update-route-metadata existing metadata now))
                  (new-routes (cons updated
                                    (remove existing routes :test #'eq))))
             (save-routes-to-disk new-routes)
             (values updated new-routes nil))
           (let* ((code (allocate-route-code routes code-length))
                  (route (make-route route-id code identity metadata now))
                  (new-routes (cons route routes)))
             (save-routes-to-disk new-routes)
             (values route new-routes t)))))))

(defun mark-route-used (route &key (now (now-iso8601)) (direct-p t))
  (call-with-routes-lock
   (lambda ()
     (let* ((routes (load-routes-from-disk))
            (route-id (getf route :route-id))
            (existing (and route-id (find-route-by-id routes route-id))))
       (unless existing
         (error "Route no longer exists for code ~a." (getf route :code)))
         (let* ((updated (copy-list existing))
                (new-routes nil))
           (setf (getf updated :last-used-at) now)
           (when direct-p
             (setf (getf updated :last-direct-used-at) now))
           (setf new-routes (cons updated (remove existing routes :test #'eq)))
         (save-routes-to-disk new-routes)
         updated)))))
