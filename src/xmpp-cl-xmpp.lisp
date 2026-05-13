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

(defun make-scram-client-nonce ()
  (string-downcase
   (ironclad:byte-array-to-hex-string (ironclad:random-data 18))))

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
  (let ((client-nonce (make-scram-client-nonce)))
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

(defun xml-attribute-value (element name)
  (let ((attribute (and element
                        (xmpp:get-attribute element name))))
    (and attribute
         (xmpp:value attribute))))

(defun xml-child-text (element name)
  (let* ((child (and element
                     (xmpp:get-element element name)))
         (text (and child
                    (xmpp:get-element child :\#text))))
    (and text
         (xmpp:data text))))

(defun xml-namespace (element)
  (xml-attribute-value element :xmlns))

(defun xml-query-namespace (element)
  (xml-namespace (and element
                      (xmpp:get-element element :query))))

(defun event-xml-element (event)
  (and (typep event 'xmpp:event)
       (xmpp:xml-element event)))

(defun xml-attribute-value-any (element names)
  (loop for name in names
        for value = (xml-attribute-value element name)
        when value
          return value))

(defun xml-query (element)
  (and element
       (xmpp:get-element element :query)))

(defun xml-elements (element name)
  (and element
       (xmpp::get-elements element name)))

(defun disco-identity-plists (query)
  (loop for identity in (xml-elements query :identity)
        collect (list :category (xml-attribute-value identity :category)
                      :type (xml-attribute-value-any identity
                                                     '(:type :type-))
                      :name (xml-attribute-value identity :name))))

(defun disco-feature-vars (query)
  (loop for feature in (xml-elements query :feature)
        for var = (xml-attribute-value feature :var)
        when var
          collect var))

(defun disco-item-plists (query)
  (loop for item in (xml-elements query :item)
        collect (list :jid (xml-attribute-value item :jid)
                      :name (xml-attribute-value item :name)
                      :node (xml-attribute-value item :node))))

(defun data-form-fields (x)
  (loop for field in (xml-elements x :field)
        collect (list :var (xml-attribute-value field :var)
                      :type (xml-attribute-value-any field '(:type :type-))
                      :values (loop for value in (xml-elements field :value)
                                    for text = (and value
                                                    (xmpp:get-element value :\#text))
                                    collect (or (and text (xmpp:data text))
                                                "")))))

(defun query-data-form-fields (query)
  (let ((x (find-if (lambda (candidate)
                      (string= (or (xml-namespace candidate) "")
                               "jabber:x:data"))
                    (xml-elements query :x))))
    (and x (data-form-fields x))))

(defun muc-user-element (element)
  (find-if (lambda (candidate)
             (string= (or (xml-namespace candidate) "")
                      "http://jabber.org/protocol/muc#user"))
           (xml-elements element :x)))

(defun muc-user-fields (element)
  (let* ((x (muc-user-element element))
         (item (and x (xmpp:get-element x :item))))
    (when x
      (list :muc-user-p t
            :muc-jid (and item (xml-attribute-value item :jid))
            :muc-affiliation (and item
                                  (xml-attribute-value item :affiliation))
            :muc-role (and item (xml-attribute-value item :role))
            :muc-status-codes
            (loop for status in (xml-elements x :status)
                  for code = (xml-attribute-value status :code)
                  when code collect code)))))

(defun append-room-address-fields (stanza)
  (let ((from (getf stanza :from)))
    (if (and from (position #\/ from))
        (multiple-value-bind (room nick)
            (parse-room-occupant-jid from)
          (append stanza (list :room-jid room :room-nick nick)))
        stanza)))

(defun append-disco-fields (stanza element)
  (let* ((query (xml-query element))
         (xmlns (xml-namespace query)))
    (cond
      ((string= (or xmlns "") "http://jabber.org/protocol/disco#info")
       (append stanza
               (list :disco-identities (disco-identity-plists query)
                     :disco-features (disco-feature-vars query))))
      ((string= (or xmlns "") "http://jabber.org/protocol/disco#items")
       (append stanza
               (list :disco-items (disco-item-plists query))))
      ((string= (or xmlns "") "http://jabber.org/protocol/muc#owner")
       (append stanza
               (list :data-form-fields (query-data-form-fields query))))
      (t
       stanza))))

(defun stanza-kind (element)
  (case (xmpp:name element)
    (:message
     (if (string-equal (or (xml-attribute-value element :type) "")
                       "groupchat")
         :groupchat
         :message))
    (:presence :presence)
    (:iq :iq)
    (t :stanza)))

(defun xml-element-stanza-plist (element)
  (let ((kind (stanza-kind element)))
    (append-room-address-fields
     (append-disco-fields
      (append
       (list :kind kind
             :name (xmpp:name element)
             :from (xml-attribute-value element :from)
             :to (xml-attribute-value element :to)
             :id (xml-attribute-value element :id)
             :type (xml-attribute-value element :type)
             :body (and (member kind '(:message :groupchat))
                        (xml-child-text element :body))
             :query-xmlns (and (eq kind :iq)
                               (xml-query-namespace element))
             :raw element)
       (and (eq kind :presence)
            (muc-user-fields element)))
      element))))

(defun event-stanza-plist (event)
  (cond
    ((typep event 'xmpp:xml-element)
     (xml-element-stanza-plist event))
    ((typep event 'xmpp:message)
     (let ((kind (if (string-equal (or (xmpp:type- event) "")
                                    "groupchat")
                     :groupchat
                     :message)))
       (append-room-address-fields
        (list :kind kind
              :from (xmpp:from event)
              :to (xmpp:to event)
              :id (xmpp:id event)
              :type (xmpp:type- event)
              :body (xmpp:body event)
              :raw event))))
    ((typep event 'xmpp:presence)
     (let ((element (event-xml-element event)))
       (append-room-address-fields
        (append
         (list :kind :presence
               :from (xmpp:from event)
               :to (xmpp:to event)
               :type (xmpp:type- event)
               :raw event)
         (and element (muc-user-fields element))))))
    ((typep event 'xmpp:simple-result)
     (list :kind :iq
           :from (xmpp:from event)
           :to (xmpp:to event)
           :id (xmpp:id event)
           :type (xmpp:type- event)
           :raw event))
    (t
     (list :kind :stanza
           :raw event))))

(defmethod receive-connected-stanza ((backend cl-xmpp-backend) connection)
  (declare (ignore backend))
  (event-stanza-plist (xmpp:receive-stanza connection :dom-repr t)))

(defmethod receive-connected-message-loop ((backend cl-xmpp-backend)
                                           connection
                                           handler)
  (loop for stanza = (receive-connected-stanza backend connection)
        do (funcall handler stanza)))

(defmethod send-disco-info ((backend cl-xmpp-backend) connection to id
                            &key node)
  (declare (ignore backend))
  (xmpp:with-iq-query (connection
                       :id id
                       :xmlns "http://jabber.org/protocol/disco#info"
                       :to to
                       :node node)))

(defmethod send-disco-items ((backend cl-xmpp-backend) connection to id
                             &key node)
  (declare (ignore backend))
  (xmpp:with-iq-query (connection
                       :id id
                       :xmlns "http://jabber.org/protocol/disco#items"
                       :to to
                       :node node)))

(defmethod join-room ((backend cl-xmpp-backend) connection room-full-jid)
  (declare (ignore backend))
  (xmpp::with-xml-output (connection)
    (fxml:with-element "presence"
      (fxml:attribute "to" room-full-jid)
      (fxml:with-element "x"
        (fxml:attribute "xmlns" "http://jabber.org/protocol/muc")
        (fxml:with-element "history"
          (fxml:attribute "maxchars" "0"))))))

(defmethod request-room-config ((backend cl-xmpp-backend)
                                connection
                                room-jid
                                id)
  (declare (ignore backend))
  (xmpp:with-iq-query (connection
                       :id id
                       :type "get"
                       :to room-jid
                       :xmlns "http://jabber.org/protocol/muc#owner")))

(defun form-value-list (value)
  (cond
    ((null value) (list ""))
    ((listp value) value)
    (t (list value))))

(defun write-data-form-field (var value &optional type)
  (fxml:with-element "field"
    (when type
      (fxml:attribute "type" type))
    (when var
      (fxml:attribute "var" var))
    (dolist (item (form-value-list value))
      (fxml:with-element "value"
        (fxml:text (or item ""))))))

(defmethod submit-room-config ((backend cl-xmpp-backend)
                               connection
                               room-jid
                               id
                               fields)
  (declare (ignore backend))
  (xmpp:with-iq-query (connection
                       :id id
                       :type "set"
                       :to room-jid
                       :xmlns "http://jabber.org/protocol/muc#owner")
    (fxml:with-element "x"
      (fxml:attribute "xmlns" "jabber:x:data")
      (fxml:attribute "type" "submit")
      (dolist (field fields)
        (destructuring-bind (var value &optional type) field
          (write-data-form-field var value type))))))

(defmethod grant-room-membership ((backend cl-xmpp-backend)
                                  connection
                                  room-jid
                                  id
                                  jid)
  (declare (ignore backend))
  (xmpp:with-iq-query (connection
                       :id id
                       :type "set"
                       :to room-jid
                       :xmlns "http://jabber.org/protocol/muc#admin")
    (fxml:with-element "item"
      (fxml:attribute "affiliation" "member")
      (fxml:attribute "jid" jid))))

(defmethod send-direct-room-invite ((backend cl-xmpp-backend)
                                    connection
                                    to
                                    room-jid
                                    reason)
  (declare (ignore backend))
  (xmpp::with-xml-output (connection)
    (fxml:with-element "message"
      (fxml:attribute "to" to)
      (fxml:with-element "x"
        (fxml:attribute "xmlns" "jabber:x:conference")
        (fxml:attribute "jid" room-jid)
        (when (and reason (plusp (length reason)))
          (fxml:attribute "reason" reason))))))

(defmethod send-room-message ((backend cl-xmpp-backend)
                              connection
                              room-jid
                              body)
  (declare (ignore backend))
  (xmpp::with-xml-output (connection)
    (fxml:with-element "message"
      (fxml:attribute "to" room-jid)
      (fxml:attribute "type" "groupchat")
      (fxml:with-element "body"
        (fxml:text body)))))

(defmethod destroy-room ((backend cl-xmpp-backend)
                         connection
                         room-jid
                         id
                         &key reason)
  (declare (ignore backend))
  (xmpp:with-iq-query (connection
                       :id id
                       :type "set"
                       :to room-jid
                       :xmlns "http://jabber.org/protocol/muc#owner")
    (fxml:with-element "destroy"
      (when reason
        (fxml:with-element "reason"
          (fxml:text reason))))))

(defmethod leave-room ((backend cl-xmpp-backend) connection room-full-jid)
  (declare (ignore backend))
  (xmpp::with-xml-output (connection)
    (fxml:with-element "presence"
      (fxml:attribute "to" room-full-jid)
      (fxml:attribute "type" "unavailable"))))

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
