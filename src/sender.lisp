(in-package #:xmpp-cli/sender)

(defvar *backend-factory* #'xmpp-cli/backend/cl-xmpp:make-backend)

(defun make-backend ()
  (funcall *backend-factory*))

(defun check-profile-login (profile-plist)
  (check-login (make-backend) profile-plist))

(defun append-send-history (profile-name recipient kind body result &key error)
  (append-history :profile profile-name
                  :to recipient
                  :kind kind
                  :bytes (utf-8-byte-length body)
                  :sha256 (sha256-hex body)
                  :result result
                  :error error))

(defun maybe-append-send-history (profile-name recipient kind body result &key error)
  (handler-case
      (append-send-history profile-name recipient kind body result :error error)
    (error (condition)
      (format *error-output*
              "warning: could not record send history: ~a~%"
              condition)
      nil)))

(defun daemon-compatible-profile-p (profile-name profile-plist)
  (handler-case
      (let ((agent-profile (getf (load-agent-config) :profile))
            (control (load-control)))
        (and control
             (string= profile-name agent-profile)
             (string= profile-name (getf control :profile))
             (string= (profile-digest profile-plist)
                      (getf control :profile-digest))))
    (error ()
      nil)))

(defun send-message-with-fallback (profile-name profile-plist recipient body)
  "Return TRANSPORT and ERROR. TRANSPORT is :DAEMON or :STANDALONE on success."
  (let ((expected-profile-digest (profile-digest profile-plist)))
    (when (daemon-compatible-profile-p profile-name profile-plist)
      (multiple-value-bind (ok response error)
          (daemon-send recipient
                       body
                       :expected-profile-digest expected-profile-digest)
        (declare (ignore response error))
        (when ok
          (return-from send-message-with-fallback (values :daemon nil))))))
  (let ((send-error
          (handler-case
              (progn
                (send-text (make-backend) profile-plist recipient body)
                nil)
            (error (condition)
              condition))))
    (if send-error
        (values nil send-error)
        (values :standalone nil))))

(defun send-message-parts-with-fallback (profile-name profile-plist recipient bodies)
  "Return TRANSPORT and ERROR after sending each body in BODIES."
  (let ((last-transport nil))
    (dolist (body bodies (values last-transport nil))
      (multiple-value-bind (transport send-error)
          (send-message-with-fallback profile-name profile-plist recipient body)
        (when send-error
          (return-from send-message-parts-with-fallback
            (values nil send-error)))
        (setf last-transport transport)))))

(defun delivery-target-kind (target)
  (cond
    ((stringp target) :jid)
    ((listp target) (getf target :kind))
    (t nil)))

(defun delivery-target-direct-jid (target)
  (cond
    ((stringp target) target)
    ((not (listp target)) nil)
    ((eq (getf target :kind) :jid) (getf target :jid))
    ((eq (getf target :kind) :route) (getf target :fallback-jid))
    (t nil)))

(defun delivery-target-route-id (target)
  (and (listp target)
       (eq (getf target :kind) :route)
       (getf target :route-id)))

(defun delivery-target-label (target)
  (cond
    ((stringp target) target)
    ((not (listp target)) "<unknown>")
    ((eq (getf target :kind) :route)
     (format nil "route:~a"
             (or (getf target :route-code)
                 (getf target :route-id)
                 "unknown")))
    ((eq (getf target :kind) :jid)
     (or (getf target :jid) "<none>"))
    (t
     "<unknown>")))

(defun daemon-response-forbids-fallback-p (response)
  (and (listp response)
       (member :fallback-allowed response)
       (not (getf response :fallback-allowed))))

(defun notification-daemon-error (response fallback)
  (or (and (listp response) (getf response :error))
      fallback))

(defun send-standalone-notification (profile-plist target body)
  (let ((recipient (delivery-target-direct-jid target)))
    (unless (and (stringp recipient) (plusp (length recipient)))
      (return-from send-standalone-notification
        (values nil "No direct fallback JID is configured.")))
    (handler-case
        (progn
          (send-text (make-backend) profile-plist recipient body)
          (values :standalone nil))
      (error (condition)
        (values nil condition)))))

(defun send-notification-with-fallback (profile-name profile-plist target body)
  "Send BODY to virtual TARGET.

Route targets are resolved by the daemon to an active room when one is bound to
the route id; otherwise the daemon uses the target's direct fallback JID.
Standalone fallback can only send to a direct JID."
  (let ((expected-profile-digest (profile-digest profile-plist)))
    (when (daemon-compatible-profile-p profile-name profile-plist)
      (multiple-value-bind (ok response error)
          (if (eq (delivery-target-kind target) :route)
              (daemon-notify (delivery-target-route-id target)
                             (delivery-target-direct-jid target)
                             body
                             :expected-profile-digest expected-profile-digest)
              (daemon-send (delivery-target-direct-jid target)
                           body
                           :expected-profile-digest expected-profile-digest))
        (declare (ignore error))
        (when ok
          (return-from send-notification-with-fallback
            (values (or (getf response :target-kind) :daemon) nil)))
        (when (daemon-response-forbids-fallback-p response)
          (return-from send-notification-with-fallback
            (values nil (notification-daemon-error response
                                                   "daemon notify failed")))))))
  (send-standalone-notification profile-plist target body))

(defun send-notification-parts-with-fallback (profile-name
                                             profile-plist
                                             target
                                             bodies)
  "Return TRANSPORT and ERROR after sending each notification body in BODIES."
  (let ((last-transport nil))
    (dolist (body bodies (values last-transport nil))
      (multiple-value-bind (transport send-error)
          (send-notification-with-fallback profile-name
                                           profile-plist
                                           target
                                           body)
        (when send-error
          (return-from send-notification-parts-with-fallback
            (values nil send-error)))
        (setf last-transport transport)))))
