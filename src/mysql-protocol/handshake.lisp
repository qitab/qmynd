;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connection Phase Packets (15.2.5)
(define-packet initial-handshake-v10
  ((tag :mysql-type (integer 1) :value 10 :transient t :bind nil)
   (server-version :mysql-type (string :null))
   (connection-id :mysql-type (integer 4))
   (auth-data :mysql-type (octets 8))
   (reserved :mysql-type (integer 1) :value 0 :transient t :bind nil)
   (capability-flags :mysql-type (integer 2) :type integer)

   ;; This is interesting: the packet may end here, or continue as follows.
   (character-set :mysql-type (integer 1) :eof :end)
   (status-flags :mysql-type (integer 2))
   (capability-flags :mysql-type (integer 2)
                     :transform #'(lambda (x) (ash x 16))
                     :reduce #'logior)

   ;; Strictly speaking, this is 0 unless +mysql-capability-client-plugin-auth+, but the field it is
   ;; used for has a length of at least 13. We're being slightly looser than the spec here, but it
   ;; should not be a problem.
   (auth-data-length :mysql-type (integer 1)
                     :transform #'(lambda (x) (max (- x 8) 13))
                     :transient t)

   (reserved :mysql-type (integer 10) :transient t :bind nil) ; reserved for future use.

   (auth-data :mysql-type (octets auth-data-length)
              :predicate (flagsp +mysql-capability-client-secure-connection+ capability-flags)
              ;; can we simplify the transform?
              :reduce #'(lambda (x y) (concatenate'(vector (unsigned-byte 8)) x y)))

   (auth-plugin :mysql-type (string :null-eof)
                :predicate (flagsp +mysql-capability-client-plugin-auth+ capability-flags))))

(defun process-initial-handshake-v10 (stream)
  "Parse an INITIAL-HANDSHAKE-V10 Packet and populate the default MySQL connection."
  (let ((packet (parse-initial-handshake-v10 stream)))
    (setf (mysql-connection-server-version *mysql-connection*)
          (initial-handshake-v10-packet-server-version packet)

          (mysql-connection-connection-id *mysql-connection*)
          (initial-handshake-v10-packet-connection-id packet)

          (mysql-connection-capabilities *mysql-connection*)
          (logand (mysql-connection-capabilities *mysql-connection*)
                  (initial-handshake-v10-packet-capability-flags packet))

          (mysql-connection-character-set *mysql-connection*)
          (mysql-cs-coll-to-character-encoding
           (initial-handshake-v10-packet-character-set packet))

          (mysql-connection-cs-coll *mysql-connection*)
          (initial-handshake-v10-packet-character-set packet)

          (mysql-connection-status-flags *mysql-connection*)
          (initial-handshake-v10-packet-status-flags packet)

          (mysql-connection-auth-data *mysql-connection*)
          (initial-handshake-v10-packet-auth-data packet)

          (mysql-connection-auth-plugin *mysql-connection*)
          (initial-handshake-v10-packet-auth-plugin packet))

    (unless (mysql-has-capability +mysql-capabilities-required+)
      (error 'mysql-insufficient-capabilities
             :server-flags (initial-handshake-v10-packet-capability-flags packet)))

    ;; asedeno-TODO: add optional logging/debugging functionality
    #+nil
    (format t "Auth Data: ~A~%~
               Auth Plugin: ~A~%~
               Server Capabilities: ~8,'0X~%~
               Client Capabilities: ~8,'0X~%~
               Combined Capabiltiies ~8,'0X~%~%"
            auth-data
            (babel:octets-to-string auth-plugin-name)
            capability-flags
            (mysql-capabilities-supported)
            (mysql-connection-capabilities *mysql-connection*))
    (values)))

(defun send-ssl-request-packet (verify)
  "Sends a MySQL SSL Connection Request Packet and begins SSL negotiation."
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-fixed-length-integer (mysql-connection-capabilities *mysql-connection*) 4 s)
     (write-fixed-length-integer #x1000000 4 s)
     (write-byte (mysql-connection-cs-coll *mysql-connection*) s)
     (write-fixed-length-integer 0 23 s))) ; 23 reserved bytes
  ;; We may not have CL+SSL, in which case we'll never get to this function,
  ;; but we still want it to compile.
  (let ((stream (uiop/package:symbol-call
                 :cl+ssl :make-ssl-client-stream
                 (mysql-connection-stream *mysql-connection*))))
    (when verify
      (uiop/package:symbol-call
       :cl+ssl :ssl-stream-check-verify
       stream))
    (setf (mysql-connection-stream *mysql-connection*) stream)))

;; We won't receive a Handshake Response packet (being a client only, not a server), but it looks like this.

;; (define-packet handshake-response-v41
;;   ((capability-flags :mysql-type (integer 4))
;;    (max-packet-size :mysql-type (integer 4))
;;    (character-set :mysql-type (integer 1))
;;    (reserved :mysql-type (string 23) :transient t :bind nil)
;;    (username :mysql-type (string :null))
;;    (auth-response-length
;;     :mysql-type (integer :lenenc)
;;     :predicate (flagsp +mysql-capability-client-plugin-auth-lenenc-client-data+ capability-flags)
;;     :transient t)
;;    (auth-response-length
;;     :mysql-type (integer 1)
;;     :predicate (and (flagsp +mysql-capability-client-secure-connection+ capability-flags)
;;                     (not (flagsp +mysql-capability-client-plugin-auth-lenenc-client-data+ capability-flags)))
;;     :transient t)
;;    (auth-response
;;     :mysql-type (octets auth-response-length)
;;     :predicate (flagsp +mysql-capability-client-secure-connection+ capability-flags))
;;    (auth-response
;;     :mysql-type (octets :null)
;;     :predicate (not (flagsp +mysql-capability-client-secure-connection+ capability-flags)))
;;    (schema
;;     :mysql-type (string :null)
;;     :predicate (flagsp +mysql-capability-client-connect-with-db+ capability-flags))
;;    (auth-plugin
;;     :mysql-type (string :null)
;;     :predicate (flagsp +mysql-capability-client-plugin-auth+ capability-flags))
;;    ;; requires additional parsing
;;    (client-capability-string
;;     :mysql-type (octets :lenenc)
;;     :predicate (flagsp +mysql-capability-client-connect-attrs+ capability-flags))))

(defun send-handshake-response-41 (&key username auth-plugin auth-response database)
  "Send a MySQL Handshake Response v41 to the default MySQL connection."
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-fixed-length-integer (mysql-connection-capabilities *mysql-connection*) 4 s)
     (write-fixed-length-integer #x1000000 4 s)
     (write-byte (mysql-connection-cs-coll *mysql-connection*) s)
     (write-fixed-length-integer 0 23 s) ; 23 reserved bytes
     (write-null-terminated-string (babel:string-to-octets username) s)
     (cond
       ((mysql-has-capability +mysql-capability-client-plugin-auth-lenec-client-data+)
        (write-length-encoded-integer (length auth-response) s)
        (write-sequence auth-response s))
       ((mysql-has-capability +mysql-capability-client-secure-connection+)
        (write-byte (length auth-response) s)
        (write-sequence auth-response s))
       (T
        (write-null-terminated-string auth-response s)))
     ;; If the bit is still set at this point, then we have a database schema to specify.
     (when (mysql-has-capability +mysql-capability-client-connect-with-db+)
       (write-null-terminated-string (babel:string-to-octets database) s))
     (when (mysql-has-capability +mysql-capability-client-plugin-auth+)
       (write-null-terminated-string auth-plugin s))
     #+mysql-client-connect-attributes
     (when (mysql-has-capability +mysql-capability-client-connect-attrs+)
       ;; asedeno-TODO: When this is implemented, what sort of
       ;; attributes do we want to send? Are they hard-coded? Supplied
       ;; by the user? Both? Stored in the connection object?
       nil))))

(defun process-initial-handshake-payload (stream)
  "Initial handshake processing dispatch."
  (ecase (peek-first-byte stream)
    (10 (process-initial-handshake-v10 stream))))


(defun mysql-connect-do-handshake (connection username password database
                                   &key compress ssl ssl-verify)
  "Perform the MySQL Initial Handshake with CONNECTION."
  ;; Read a wire packet
  (let ((initial-handshake-payload (mysql-connection-read-packet connection)))
    (with-mysql-connection (connection)
      ;; Process Initial Handshake
      (process-initial-handshake-payload initial-handshake-payload)

      (unless database
        (setf (mysql-connection-capabilities connection)
              (logandc2 (mysql-connection-capabilities connection)
                        +mysql-capability-client-connect-with-db+)))

      (unless compress
        (setf (mysql-connection-capabilities connection)
              (logandc2 (mysql-connection-capabilities connection)
                        +mysql-capability-client-compress+)))

      ;; Deal with SSL
      (unless (eq ssl :unspecified)
        (cond
          ((and ssl (not (mysql-has-capability +mysql-capability-client-ssl+)))
           ;; SSL requested, but we don't have it.
           (error 'ssl-not-supported))
          ((null ssl)
           ;; SSL explicitly disabled
           (setf (mysql-connection-capabilities connection)
                 (logandc2 (mysql-connection-capabilities connection)
                           +mysql-capability-client-ssl+)))))
      (when (mysql-has-capability +mysql-capability-client-ssl+)
        (send-ssl-request-packet ssl-verify))

      ;; Prepare Auth Response
      (handler-case
          (with-prefixed-accessors (auth-data auth-plugin)
            (mysql-connection- connection)
            ;; Prepare Initial Response OR Close and Signal
            (send-handshake-response-41 :username username
                                        :auth-response (generate-auth-response password auth-data auth-plugin)
                                        :auth-plugin auth-plugin
                                        :database database)
            (parse-response (mysql-read-packet)))
        (mysql-base-error (e)
          (mysql-connection-close-socket connection)
          (error e)))

      ;; Enable compression if possible
      (when (mysql-has-capability +mysql-capability-client-compress+)
        (setf (mysql-connection-stream connection)
              (make-instance 'mysql-compressed-stream
                             :stream (mysql-connection-stream connection)))))

    (setf (mysql-connection-connected connection) t)
    connection))
