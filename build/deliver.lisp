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
  (let ((entry-point (intern "ENTRY-POINT" "XMPP-CLI/MAIN")))
    (deliver entry-point
             (merge-pathnames "build/xmpp-cli" root)
             0
             :multiprocessing nil))

  #-lispworks
  (error "build/deliver.lisp must be run with LispWorks."))
