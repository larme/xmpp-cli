(in-package #:xmpp-cli/test)

(defun make-xmpp-mechanism (name)
  (make-instance 'xmpp:xml-element
                 :name :mechanism
                 :elements
                 (list (make-instance 'xmpp:xml-element
                                      :name :\#text
                                      :data name))))

(deftest scram-rfc5802-vector
  (multiple-value-bind (client-final-message server-signature)
      (xmpp-cli/backend/cl-xmpp::make-scram-client-final-message
       "pencil"
       "fyko+d2lbbFgONRv9qkxdawL"
       "n=user,r=fyko+d2lbbFgONRv9qkxdawL"
       "r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096")
    (check-equal
     "c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="
     client-final-message)
    (check-equal "rmF9pqV8S7suAoZWja4dJRkFsKQ=" server-signature)
    (check
     (xmpp-cli/backend/cl-xmpp::verify-scram-server-final
      "v=rmF9pqV8S7suAoZWja4dJRkFsKQ="
      server-signature)
     "SCRAM server signature should verify.")))

(deftest scram-escapes-sasl-name
  (check-equal "a=2Cb=3Dc"
               (xmpp-cli/backend/cl-xmpp::scram-sasl-name "a,b=c")))

(deftest auto-mechanism-prefers-scram
  (let ((connection (make-instance 'xmpp:connection)))
    (setf (xmpp:mechanisms connection)
          (list (make-xmpp-mechanism "SCRAM-SHA-1-PLUS")
                (make-xmpp-mechanism "SCRAM-SHA-1")
                (make-xmpp-mechanism "PLAIN")))
    (check-equal :scram-sha-1
                 (xmpp-cli/backend/cl-xmpp::resolve-mechanism
                  connection
                  :auto))))

(deftest mechanism-aliases-resolve-to-sasl
  (let ((connection (make-instance 'xmpp:connection)))
    (setf (xmpp:mechanisms connection)
          (list (make-xmpp-mechanism "PLAIN")
                (make-xmpp-mechanism "DIGEST-MD5")
                (make-xmpp-mechanism "SCRAM-SHA-1")))
    (check-equal :sasl-plain
                 (xmpp-cli/backend/cl-xmpp::resolve-mechanism
                  connection
                  :plain))
    (check-equal :sasl-digest-md5
                 (xmpp-cli/backend/cl-xmpp::resolve-mechanism
                  connection
                  :digest-md5))
    (check-equal :scram-sha-1
                 (xmpp-cli/backend/cl-xmpp::resolve-mechanism
                  connection
                  :sasl-scram-sha-1))))

(deftest scram-plus-reports-channel-binding-gap
  (let ((connection (make-instance 'xmpp:connection)))
    (setf (xmpp:mechanisms connection)
          (list (make-xmpp-mechanism "SCRAM-SHA-1-PLUS")))
    (check-signals-error
      (xmpp-cli/backend/cl-xmpp::resolve-mechanism
       connection
       :scram-sha-1-plus))))
