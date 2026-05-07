(in-package #:xmpp-cli/history)

(defun history-pathname ()
  (merge-pathnames "history.sexp" (home-xmpp-cli-directory)))

(defun history-temp-pathname ()
  (merge-pathnames "history.sexp.tmp" (home-xmpp-cli-directory)))

(defun print-sexpression (object)
  (with-output-to-string (out)
    (write object
           :stream out
           :case :downcase
           :circle nil
           :pretty nil
           :readably t)
    (terpri out)))

(defun read-history-file (pathname)
  (with-open-file (in pathname
                      :direction :input
                      :element-type 'character
                      :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (eof (list :eof)))
        (let ((object (read in nil eof)))
          (cond
            ((eq object eof) nil)
            ((listp object) object)
            (t (error "Malformed history.sexp: expected a list, got ~s" object))))))))

(defun load-history ()
  (let ((pathname (history-pathname)))
    (if (probe-file pathname)
        (read-history-file pathname)
        nil)))

(defun save-history (history)
  (ensure-private-directory)
  (let ((temp (history-temp-pathname))
        (target (history-pathname)))
    (write-private-file temp (print-sexpression history))
    (uiop:rename-file-overwriting-target temp target)
    target))

(defun append-history (&key profile to kind body bytes sha256 result error)
  (declare (ignore body))
  (let* ((entry (append (list :time (now-iso8601)
                              :profile profile
                              :to to
                              :kind kind
                              :bytes (or bytes 0)
                              :sha256 (or sha256 "")
                              :result result)
                        (when error
                          (list :error error))))
         (history (append (load-history) (list entry))))
    (save-history history)
    entry))
