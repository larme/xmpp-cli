(in-package #:xmpp-cli/test)

(defvar *tests* nil)

(defmacro deftest (name &body body)
  `(push (cons ',name (lambda () ,@body)) *tests*))

(defun check (condition control &rest arguments)
  (unless condition
    (error "Test assertion failed: ~?" control arguments)))

(defmacro check-equal (expected form)
  `(let ((expected-value ,expected)
         (actual-value ,form))
     (check (equal expected-value actual-value)
            "expected ~s, got ~s"
            expected-value
            actual-value)))

(defmacro check-signals-error (&body body)
  `(let ((signaled nil))
     (handler-case
         (progn ,@body)
       (error ()
         (setf signaled t)))
     (check signaled "Expected an error, but no error was signaled.")
     t))

(defun make-test-directory ()
  (let ((directory (merge-pathnames
                    (format nil "xmpp-cli-test-~36r-~36r/"
                            (get-universal-time)
                            (random 1000000))
                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defmacro with-isolated-data (&body body)
  `(let ((xmpp-cli/util::*data-directory* (make-test-directory)))
     ,@body))

(defun run-tests ()
  (let ((failures 0))
    (dolist (test (reverse *tests*))
      (handler-case
          (progn
            (funcall (cdr test))
            (format t "~&PASS ~a~%" (car test)))
        (error (condition)
          (incf failures)
          (format *error-output* "~&FAIL ~a: ~a~%" (car test) condition))))
    (when (plusp failures)
      (error "~d test~:p failed." failures))
    (format t "~&~d tests passed.~%" (length *tests*))
    t))
