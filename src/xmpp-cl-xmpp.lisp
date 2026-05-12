(in-package #:xmpp-cli/backend/cl-xmpp)

(defclass cl-xmpp-backend () ())

(defun make-backend ()
  (make-instance 'cl-xmpp-backend))

(defun required (plist key)
  (or (getf plist key)
      (error "Missing required profile key ~s" key)))

(defun string-to-base64 (string)
  (base64:usb8-array-to-base64-string (xmpp-cli/util:utf-8-octets string)))

(defun ascii-octets (string)
  (ironclad:ascii-string-to-byte-array string))

(defun split-scram-fields (message)
  (let ((fields nil)
        (start 0))
    (loop
      (let ((position (position #\, message :start start)))
        (push (subseq message start position) fields)
        (unless position
          (return (nreverse fields)))
        (setf start (1+ position))))))

(defun parse-scram-attributes (message)
  (mapcar (lambda (field)
            (let ((position (position #\= field)))
              (unless position
                (error "Malformed SCRAM field in server message: ~a" field))
              (cons (subseq field 0 position)
                    (subseq field (1+ position)))))
          (split-scram-fields message)))

(defun scram-attribute (attributes name &key required)
  (let ((entry (assoc name attributes :test #'string=)))
    (cond
      (entry (cdr entry))
      (required
       (error "SCRAM server message is missing required attribute ~a." name))
      (t nil))))

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun scram-sasl-name (name)
  (with-output-to-string (out nil :element-type 'character)
    (loop for char across name
          do (case char
               (#\, (write-string "=2C" out))
               (#\= (write-string "=3D" out))
               (t (write-char char out))))))

(defun make-scram-client-first-message (username nonce)
  (let ((client-first-bare (format nil "n=~a,r=~a"
                                   (scram-sasl-name username)
                                   nonce)))
    (values (concatenate 'string "n,," client-first-bare)
            client-first-bare)))

(defun hmac-sha1 (key message)
  (ironclad:hmac-digest
   (ironclad:update-hmac
    (ironclad:make-hmac key :sha1)
    message)))

(defun xor-octet-vectors (left right)
  (unless (= (length left) (length right))
    (error "Cannot XOR octet vectors of different lengths: ~d and ~d."
           (length left)
           (length right)))
  (let ((result (make-array (length left) :element-type '(unsigned-byte 8))))
    (dotimes (index (length left) result)
      (setf (aref result index)
            (logxor (aref left index) (aref right index))))))

(defun make-scram-client-final-message (password
                                        client-nonce
                                        client-first-bare
                                        server-first-message)
  (let* ((attributes (parse-scram-attributes server-first-message))
         (server-nonce (scram-attribute attributes "r" :required t))
         (salt (base64:base64-string-to-usb8-array
                (scram-attribute attributes "s" :required t)))
         (iterations (parse-integer
                      (scram-attribute attributes "i" :required t)))
         (channel-binding (string-to-base64 "n,,"))
         (client-final-bare (format nil "c=~a,r=~a"
                                    channel-binding
                                    server-nonce)))
    (unless (string-prefix-p client-nonce server-nonce)
      (error "SCRAM server nonce does not start with the client nonce."))
    (unless (plusp iterations)
      (error "SCRAM server supplied a non-positive iteration count: ~d."
             iterations))
    (let* ((salted-password
             (ironclad:pbkdf2-hash-password
              (xmpp-cli/util:utf-8-octets password)
              :salt salt
              :digest :sha1
              :iterations iterations))
           (auth-message (format nil "~a,~a,~a"
                                 client-first-bare
                                 server-first-message
                                 client-final-bare))
           (auth-message-octets (xmpp-cli/util:utf-8-octets auth-message))
           (client-key (hmac-sha1 salted-password
                                  (ascii-octets "Client Key")))
           (stored-key (ironclad:digest-sequence :sha1 client-key))
           (client-signature (hmac-sha1 stored-key auth-message-octets))
           (client-proof (xor-octet-vectors client-key client-signature))
           (server-key (hmac-sha1 salted-password
                                  (ascii-octets "Server Key")))
           (server-signature (hmac-sha1 server-key auth-message-octets))
           (client-final-message
             (format nil "~a,p=~a"
                     client-final-bare
                     (base64:usb8-array-to-base64-string client-proof))))
      (values client-final-message
              (base64:usb8-array-to-base64-string server-signature)))))

(defun constant-time-string= (left right)
  (and (= (length left) (length right))
       (ironclad:constant-time-equal (ascii-octets left)
                                     (ascii-octets right))))

(defun verify-scram-server-final (server-final-message expected-signature)
  (let* ((attributes (parse-scram-attributes server-final-message))
         (server-error (scram-attribute attributes "e"))
         (server-signature (scram-attribute attributes "v")))
    (cond
      (server-error
       (error "SCRAM server returned an authentication error: ~a"
              server-error))
      ((null server-signature)
       (error "SCRAM success response did not include a server signature."))
      ((not (constant-time-string= expected-signature server-signature))
       (error "SCRAM server signature did not match."))
      (t t))))

(defun sasl-element-text (element)
  (let ((text (xmpp:get-element element :\#text)))
    (or (and text (xmpp:data text)) "")))

(defun decode-sasl-payload (element)
  (let ((payload (sasl-element-text element)))
    (if (zerop (length payload))
        ""
        (base64:base64-string-to-string payload))))

(defun send-sasl-auth (connection mechanism initial-response)
  (xmpp::with-xml-stream (stream connection)
    (xmpp::xml-output
     stream
     (format nil "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='~a'>~a</auth>"
             mechanism
             (string-to-base64 initial-response)))))

(defun send-sasl-response (connection response)
  (xmpp::with-xml-stream (stream connection)
    (xmpp::xml-output
     stream
     (format nil "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>~a</response>"
             (string-to-base64 response)))))

(defun receive-sasl-stanza (connection)
  (xmpp:receive-stanza connection :dom-repr t))

(defun scram-sha-1-authenticate (connection username password)
  (let ((client-nonce (cl-scram:gen-client-nonce)))
    (multiple-value-bind (client-first-message client-first-bare)
        (make-scram-client-first-message username client-nonce)
      (send-sasl-auth connection "SCRAM-SHA-1" client-first-message)
      (let ((server-first-stanza (receive-sasl-stanza connection)))
        (case (xmpp:name server-first-stanza)
          (:challenge
           (let ((server-first-message (decode-sasl-payload server-first-stanza)))
             (multiple-value-bind (client-final-message expected-server-signature)
                 (make-scram-client-final-message password
                                                  client-nonce
                                                  client-first-bare
                                                  server-first-message)
               (send-sasl-response connection client-final-message)
               (let ((server-final-stanza (receive-sasl-stanza connection)))
                 (case (xmpp:name server-final-stanza)
                   (:success
                    (verify-scram-server-final
                     (decode-sasl-payload server-final-stanza)
                     expected-server-signature)
                    :authentication-successful)
                   (t server-final-stanza))))))
          (t server-first-stanza))))))

(defun %sasl-scram-sha-1% (connection username password resource)
  (declare (ignore resource))
  (xmpp::if-successful-restart-stream
   connection
   (scram-sha-1-authenticate connection username password)))

(defun register-cl-xmpp-auth-method (name operator)
  (let ((entry (assoc name xmpp::*auth-methods*)))
    (if entry
        (setf (second entry) operator)
        (xmpp::add-auth-method name operator))))

(register-cl-xmpp-auth-method :scram-sha-1 '%sasl-scram-sha-1%)
(register-cl-xmpp-auth-method :sasl-scram-sha-1 '%sasl-scram-sha-1%)

(defun mechanism-xmpp-name (mechanism)
  (case mechanism
    ((:sasl-plain :plain) "PLAIN")
    ((:sasl-digest-md5 :digest-md5) "DIGEST-MD5")
    ((:scram-sha-1 :sasl-scram-sha-1) "SCRAM-SHA-1")
    ((:scram-sha-1-plus :sasl-scram-sha-1-plus) "SCRAM-SHA-1-PLUS")
    (t (string-upcase (string mechanism)))))

(defun supported-mechanism-p (mechanism)
  (member mechanism '(:sasl-plain
                      :plain
                      :sasl-digest-md5
                      :digest-md5
                      :scram-sha-1
                      :sasl-scram-sha-1)))

(defun canonical-sasl-mechanism (mechanism)
  (case mechanism
    ((:plain :sasl-plain) :sasl-plain)
    ((:digest-md5 :sasl-digest-md5) :sasl-digest-md5)
    ((:scram-sha-1 :sasl-scram-sha-1) :scram-sha-1)
    (t mechanism)))

(defun scram-plus-mechanism-p (mechanism)
  (member mechanism '(:scram-sha-1-plus :sasl-scram-sha-1-plus)))

(defun auto-mechanism-p (mechanism)
  (eq mechanism :auto))

(defun mechanism-text (element)
  (let ((text (xmpp:get-element element :\#text)))
    (and text (xmpp:data text))))

(defun advertised-mechanisms (connection)
  (remove nil (mapcar #'mechanism-text (xmpp:mechanisms connection))))

(defun advertised-mechanism-p (advertised mechanism-name)
  (member mechanism-name advertised :test #'string-equal))

(defun select-automatic-mechanism (advertised)
  (cond
    ((advertised-mechanism-p advertised "SCRAM-SHA-1") :scram-sha-1)
    ((advertised-mechanism-p advertised "PLAIN") :sasl-plain)
    ((advertised-mechanism-p advertised "DIGEST-MD5") :sasl-digest-md5)
    (t (error "Server does not advertise a supported SASL mechanism; advertised mechanisms: ~{~a~^, ~}"
              advertised))))

(defun resolve-mechanism (connection mechanism)
  (let ((advertised (advertised-mechanisms connection)))
    (cond
      ((auto-mechanism-p mechanism)
       (unless advertised
         (error "Server did not advertise any SASL mechanisms."))
       (select-automatic-mechanism advertised))
      ((scram-plus-mechanism-p mechanism)
       (error "SCRAM-SHA-1-PLUS requires TLS channel binding data, which this cl+ssl backend does not expose yet. Use --mechanism scram-sha-1 when the server advertises SCRAM-SHA-1."))
      ((not (supported-mechanism-p mechanism))
       (error "The cl-xmpp backend does not support SASL mechanism ~a." mechanism))
      (t
       (let ((wanted (mechanism-xmpp-name mechanism)))
         (when (and advertised
                    (not (advertised-mechanism-p advertised wanted)))
           (error "Server does not advertise SASL mechanism ~a; advertised mechanisms: ~{~a~^, ~}"
                  wanted
                  advertised)))
       (canonical-sasl-mechanism mechanism)))))

(defmacro with-cl-xmpp-connection ((connection profile &key
                                               (send-presence nil)
                                               resource)
                                   &body body)
  `(call-with-cl-xmpp-connection
    ,profile
    (lambda (,connection) ,@body)
    :send-presence ,send-presence
    :resource ,resource))

(defun call-with-cl-xmpp-connection (profile thunk &key send-presence resource)
  (let* ((username (required profile :username))
         (password (required profile :password))
         (domain (required profile :domain))
         (host (or (getf profile :host) domain))
         (port (or (getf profile :port) 5222))
         (resource (or resource (getf profile :resource) "xmpp-cli"))
         (mechanism (or (getf profile :mechanism) :auto))
         (connection nil))
    (let ((xmpp:*debug-stream* nil))
      (unwind-protect
           (progn
             (multiple-value-bind (new-connection tls-status tls-reply)
                 (xmpp:connect-tls :hostname host
                                   :port port
                                   :jid-domain-part domain)
               (setf connection new-connection)
               (when (and tls-status (not (eq tls-status :proceed)))
                 (error "XMPP TLS negotiation failed: ~s ~s" tls-status tls-reply)))
             (setf mechanism (resolve-mechanism connection mechanism))
             (let ((result (xmpp:auth connection
                                      username
                                      password
                                      resource
                                      :mechanism mechanism
                                      :send-presence send-presence)))
               (unless (eq result :authentication-successful)
                 (error "XMPP authentication failed: ~s" result)))
             (funcall thunk connection))
        (when connection
          (ignore-errors
            (xmpp:end-xml-stream connection))
          (ignore-errors
            (xmpp:disconnect connection)))))))

(defmethod check-login ((backend cl-xmpp-backend) profile)
  (declare (ignore backend))
  (with-cl-xmpp-connection (connection profile :send-presence nil)
    (declare (ignore connection))
    t))

(defmethod call-with-connection ((backend cl-xmpp-backend)
                                 profile
                                 function
                                 &key
                                   resource
                                   send-presence)
  (declare (ignore backend))
  (call-with-cl-xmpp-connection profile
                                function
                                :resource resource
                                :send-presence send-presence))

(defmethod send-connected-text ((backend cl-xmpp-backend) connection to body)
  (declare (ignore backend))
  (xmpp:message connection to body :type :chat)
  :sent)

(defmethod receive-connected-message-loop ((backend cl-xmpp-backend)
                                           connection
                                           handler)
  (declare (ignore backend))
  (loop for event = (xmpp:receive-stanza connection)
        do (when (typep event 'xmpp:message)
             (let ((body (xmpp:body event)))
               (when (and body (plusp (length body)))
                 (funcall handler
                          (list :from (xmpp:from event)
                                :to (xmpp:to event)
                                :type (xmpp:type- event)
                                :body body)))))))

(defmethod close-connection ((backend cl-xmpp-backend) connection)
  (declare (ignore backend))
  (ignore-errors
    (xmpp:end-xml-stream connection))
  (ignore-errors
    (xmpp:disconnect connection))
  t)

(defmethod send-text ((backend cl-xmpp-backend) profile to body)
  (call-with-connection
   backend
   profile
   (lambda (connection)
     (send-connected-text backend connection to body))
   :send-presence nil))
