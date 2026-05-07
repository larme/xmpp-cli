(in-package #:xmpp-cli/main)

(defun entry-point ()
  #+lispworks
  (handler-case
      (let ((argv (rest sys:*line-arguments-list*)))
        (lispworks:quit
         :status (xmpp-cli/cli:run argv)
         :ignore-errors-p t))
    (error (condition)
      (format *error-output* "xmpp-cli: ~a~%" condition)
      (finish-output *error-output*)
      (lispworks:quit :status 1 :ignore-errors-p t)))
  #-lispworks
  (xmpp-cli/cli:run (uiop:command-line-arguments)))
