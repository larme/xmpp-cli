(in-package #:xmpp-cli/agent-daemon)

(defstruct daemon-state
  backend
  profile-name
  profile
  agent-config
  control
  server-socket
  token
  (stop-p nil)
  (xmpp-status :starting)
  connection
  connected-at
  last-error
  (lock (bt:make-lock "xmpp-cli agent daemon")))

(defparameter *reply-whitespace* '(#\Space #\Tab #\Newline #\Return))
(defparameter *daemon-thread-stop-wait-seconds* 5)
(defparameter *daemon-client-timeout-seconds* 2)

(defun state-stopped-p (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (daemon-state-stop-p state)))

(defun wake-control-server (server)
  (let ((port (and server
                   (ignore-errors
                     (usocket:get-local-port server)))))
    (when port
      (ignore-errors
        (let ((client (usocket:socket-connect "127.0.0.1"
                                              port
                                              :element-type '(unsigned-byte 8)
                                              :timeout 1)))
          (usocket:socket-close client))))))

(defun request-stop (state)
  (let ((connection nil)
        (server nil))
    (bt:with-lock-held ((daemon-state-lock state))
      (setf (daemon-state-stop-p state) t)
      (setf connection (daemon-state-connection state))
      (setf server (daemon-state-server-socket state)))
    (when server
      (wake-control-server server)
      (ignore-errors
        (usocket:socket-close server)))
    (when connection
      (ignore-errors
        (close-connection (daemon-state-backend state) connection)))))

(defun mark-connecting (state)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (daemon-state-xmpp-status state) :connecting)
    (setf (daemon-state-connection state) nil)
    (setf (daemon-state-connected-at state) nil)))

(defun mark-connected (state connection)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (daemon-state-xmpp-status state) :connected)
    (setf (daemon-state-connection state) connection)
    (setf (daemon-state-connected-at state) (now-iso8601))
    (setf (daemon-state-last-error state) nil)))

(defun mark-disconnected (state error-text)
  (bt:with-lock-held ((daemon-state-lock state))
    (setf (daemon-state-xmpp-status state)
          (if (daemon-state-stop-p state) :stopped :disconnected))
    (setf (daemon-state-connection state) nil)
    (setf (daemon-state-connected-at state) nil)
    (setf (daemon-state-last-error state) error-text)))

(defun route-ttl-days (state)
  (getf (daemon-state-agent-config state) :route-ttl-days))

(defun daemon-send-text (state to body)
  (let ((backend (daemon-state-backend state))
        (connection nil))
    (bt:with-lock-held ((daemon-state-lock state))
      (unless (and (eq (daemon-state-xmpp-status state) :connected)
                   (daemon-state-connection state))
        (return-from daemon-send-text
          (values nil "XMPP daemon connection is not ready.")))
      (setf connection (daemon-state-connection state))
      (handler-case
          (progn
            (send-connected-text backend connection to body)
            (values t nil))
        (error (condition)
          (let ((text (princ-to-string condition)))
            (setf (daemon-state-xmpp-status state) :disconnected)
            (setf (daemon-state-last-error state) text)
            (setf (daemon-state-connection state) nil)
            (values nil text)))))))
