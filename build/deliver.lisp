(in-package #:cl-user)

#+lispworks
(load-all-patches)

(require "asdf")

(defun script-directory ()
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defun project-root ()
  (truename (merge-pathnames "../" (script-directory))))

(defun quicklisp-setup-candidates ()
  (remove nil
          (list
           (let ((override (uiop:getenv "QUICKLISP_SETUP")))
             (and override (pathname override)))
           (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))
           (merge-pathnames ".local/quicklisp/setup.lisp" (user-homedir-pathname))
           (merge-pathnames ".local/quicklisp/quicklisp/setup.lisp"
                            (user-homedir-pathname)))))

(defun load-quicklisp ()
  (dolist (candidate (quicklisp-setup-candidates))
    (when (probe-file candidate)
      (load candidate)
      (return-from load-quicklisp candidate)))
  (error "Quicklisp setup.lisp not found. Set QUICKLISP_SETUP or install Quicklisp."))

(defun quickload (system)
  (let* ((package (find-package :ql))
         (function (and package (find-symbol "QUICKLOAD" package))))
    (unless (and function (fboundp function))
      (error "Quicklisp package QL:QUICKLOAD is not available."))
    (funcall function system :silent nil)))

(defun parse-delivery-level ()
  (let ((raw (uiop:getenv "DELIVERY_LEVEL")))
    (if (and raw (plusp (length raw)))
        (let ((level (parse-integer raw :junk-allowed nil)))
          (unless (<= 0 level 5)
            (error "DELIVERY_LEVEL must be between 0 and 5, got ~a." raw))
          level)
        2)))

(defun env-true-p (name)
  (let ((raw (uiop:getenv name)))
    (and raw
         (not (member (string-downcase raw)
                      '("" "0" "false" "no" "nil")
                      :test #'string=)))))

(defun delivery-debug-p (level)
  (let ((raw (uiop:getenv "DELIVERY_DEBUG")))
    (if raw
        (env-true-p "DELIVERY_DEBUG")
        (zerop level))))

(let* ((root (project-root))
       (vendor-cl-xmpp (merge-pathnames "vendor/cl-xmpp/" root)))
  (load-quicklisp)

  #+lispworks
  (require "comm")

  (pushnew root asdf:*central-registry* :test #'equal)
  (when (probe-file vendor-cl-xmpp)
    (pushnew vendor-cl-xmpp asdf:*central-registry* :test #'equal))

  (quickload "xmpp-cli")

  (ensure-directories-exist (merge-pathnames "build/" root))

  #+lispworks
  (let* ((entry-point (intern "ENTRY-POINT" "XMPP-CLI/MAIN"))
         (output (merge-pathnames "build/xmpp-cli" root))
         (level (parse-delivery-level))
         (debug-p (delivery-debug-p level)))
    (format t "~&Delivering xmpp-cli at level ~d~@[ with delivery debug support~]...~%"
            level
            debug-p)
    (deliver entry-point
             output
             level
             :multiprocessing nil
             :keep-debug-mode debug-p
             :keep-stub-functions debug-p
             :keep-function-name (if debug-p t :minimal)
             :keep-conditions :all
             :keep-eval debug-p
             :keep-pretty-printer t
             :keep-lisp-reader t
             :keep-load-function (and debug-p :full)))

  #-lispworks
  (error "build/deliver.lisp must be run with LispWorks."))
