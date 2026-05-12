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
      (let ((agent-profile (getf (load-agent-config) :profile "default"))
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
