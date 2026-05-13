(in-package #:xmpp-cli/agent-rooms)

(defparameter *room-random-alphabet* "abcdefghijklmnopqrstuvwxyz")
(defparameter *room-slug-max-length* 32)
(defparameter *rooms-lock-stale-seconds* 30)
(defparameter *rooms-lock-wait-seconds* 10)

(defun rooms-pathname ()
  (merge-pathnames "rooms.yaml" (agent-directory)))

(defun rooms-lock-pathname ()
  (merge-pathnames "rooms.lock" (agent-directory)))

(defun call-with-rooms-lock (thunk)
  (ensure-agent-directory)
  (call-with-file-lock (rooms-lock-pathname)
                       thunk
                       :timeout-seconds *rooms-lock-wait-seconds*
                       :stale-seconds *rooms-lock-stale-seconds*
                       :use-mtime-p t
                       :label "agent room lock"))

(defun yaml-to-room (mapping)
  (unless (listp mapping)
    (error "Malformed agent/rooms.yaml: room entry must be a mapping."))
  (yaml-to-plist mapping :value-from-yaml #'yaml-null-to-nil))

(defun room-to-yaml (room)
  (plist-to-yaml room :value-to-yaml #'nil-to-yaml-null))

(defun load-rooms-from-disk ()
  (read-yaml-record-list-file (rooms-pathname)
                              #'yaml-to-room
                              :label "agent/rooms.yaml"))

(defun save-rooms-to-disk (rooms)
  (ensure-agent-directory)
  (write-yaml-record-list-file (rooms-pathname)
                               rooms
                               #'room-to-yaml))

(defun load-rooms ()
  (load-rooms-from-disk))

(defun save-rooms (rooms)
  (call-with-rooms-lock
   (lambda ()
     (save-rooms-to-disk rooms))))

(defun active-room-p (room)
  (string-equal "active" (or (getf room :state) "active")))

(defun active-rooms (&optional (rooms (load-rooms)))
  (remove-if-not #'active-room-p rooms))

(defun room-activity-time (room)
  (let ((times (remove nil
                       (mapcar (lambda (key)
                                 (parse-iso8601 (getf room key)))
                               '(:last-activity-at :created-at)))))
    (and times (reduce #'max times))))

(defun room-expired-p (room ttl-hours &optional (now (get-universal-time)))
  (and ttl-hours
       (let ((activity-time (room-activity-time room)))
         (or (null activity-time)
             (> (- now activity-time)
                (* ttl-hours 60 60))))))

(defun room-bare-jid (jid)
  (let ((slash (and jid (position #\/ jid))))
    (if slash (subseq jid 0 slash) jid)))

(defun room-nick (jid)
  (let ((slash (and jid (position #\/ jid))))
    (and slash (< (1+ slash) (length jid))
         (subseq jid (1+ slash)))))

(defun room-full-jid (room-jid nick)
  (format nil "~a/~a" room-jid nick))

(defun parse-room-occupant-jid (jid)
  (values (room-bare-jid jid) (room-nick jid)))

(defun find-room-by-jid (jid &optional (rooms (load-rooms)))
  (let ((bare (room-bare-jid jid)))
    (find bare rooms :test #'string-equal
          :key (lambda (room)
                 (getf room :room-jid)))))

(defun find-active-room-by-jid (jid &optional (rooms (load-rooms)))
  (find-if (lambda (room)
             (and (active-room-p room)
                  (string-equal (room-bare-jid jid)
                                (getf room :room-jid))))
           rooms))

(defun find-active-room-by-route-code (route-code &optional (rooms (load-rooms)))
  (find-if (lambda (room)
             (and (active-room-p room)
                  (string-equal route-code (getf room :route-code))))
           rooms))

(defun find-active-room-by-route-id (route-id &optional (rooms (load-rooms)))
  (find-if (lambda (room)
             (and (active-room-p room)
                  (string= route-id (or (getf room :route-id) ""))))
           rooms))

(defun upsert-room (room)
  (call-with-rooms-lock
   (lambda ()
     (let* ((rooms (load-rooms-from-disk))
            (room-jid (getf room :room-jid))
            (route-id (getf room :route-id))
            (updated (cons room
                           (remove-if
                            (lambda (existing)
                              (or (string-equal room-jid
                                                (getf existing :room-jid))
                                  (and route-id
                                       (active-room-p existing)
                                       (string= route-id
                                                (or (getf existing :route-id)
                                                    "")))))
                            rooms))))
       (save-rooms-to-disk updated)
       room))))

(defun update-room (room-jid function)
  (call-with-rooms-lock
   (lambda ()
     (let* ((rooms (load-rooms-from-disk))
            (existing (find-room-by-jid room-jid rooms)))
       (unless existing
         (error "Unknown room: ~a" room-jid))
       (let* ((updated-room (funcall function (copy-list existing)))
              (updated-rooms (cons updated-room
                                   (remove existing rooms :test #'eq))))
         (save-rooms-to-disk updated-rooms)
         updated-room)))))

(defun mark-room-activity (room &optional (now (now-iso8601)))
  (update-room (getf room :room-jid)
               (lambda (entry)
                 (setf (getf entry :last-activity-at) now)
                 entry)))

(defun mark-room-closed (room &optional (now (now-iso8601)))
  (update-room (getf room :room-jid)
               (lambda (entry)
                 (setf (getf entry :state) "closed")
                 (setf (getf entry :closed-at) now)
                 entry)))

(defun slug-char (char)
  (cond
    ((and (char>= char #\a) (char<= char #\z)) char)
    ((and (char>= char #\A) (char<= char #\Z)) (char-downcase char))
    ((and (char>= char #\0) (char<= char #\9)) char)
    (t #\-)))

(defun collapse-hyphens (text)
  (with-output-to-string (out nil :element-type 'character)
    (loop with previous-hyphen = nil
          for char across text
          for normalized = (slug-char char)
          do (cond
               ((char= normalized #\-)
                (unless previous-hyphen
                  (write-char #\- out))
                (setf previous-hyphen t))
               (t
                (write-char normalized out)
                (setf previous-hyphen nil))))))

(defun sanitize-room-slug (name)
  (let* ((collapsed (collapse-hyphens (or name "")))
         (trimmed (string-trim '(#\-) collapsed))
         (capped (if (> (length trimmed) *room-slug-max-length*)
                     (string-right-trim '(#\-) (subseq trimmed 0 *room-slug-max-length*))
                     trimmed)))
    (if (plusp (length capped)) capped "room")))

(defun random-room-suffix (&optional (length 4))
  (with-output-to-string (out nil :element-type 'character)
    (dotimes (index length)
      (declare (ignore index))
      (write-char (char *room-random-alphabet*
                        (random (length *room-random-alphabet*)))
                  out))))

(defun make-room-node (route-code room-name)
  (format nil "xmppcli-~a-~a-~a"
          (string-downcase route-code)
          (sanitize-room-slug room-name)
          (random-room-suffix)))

(defun make-room-jid (service-jid route-code room-name)
  (format nil "~a@~a"
          (make-room-node route-code room-name)
          service-jid))

(defun join-uri (room-jid)
  (format nil "xmpp:~a?join" room-jid))

(defun room-summary-line (room)
  (format nil "~a route=~a state=~a last=~a"
          (getf room :room-jid)
          (getf room :route-code)
          (or (getf room :state) "active")
          (or (getf room :last-activity-at) (getf room :created-at) "unknown")))

(defun room-summary-lines (&optional (rooms (active-rooms)))
  (mapcar #'room-summary-line rooms))
