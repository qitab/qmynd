;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mysqlnd)

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

   ;; Strictly speaking, this is 0 unless $mysql-capability-client-plugin-auth, but the field it is
   ;; used for has a length of at least 13. We're being slightly looser than the spec here, but it
   ;; should not be a problem.
   (auth-data-length :mysql-type (integer 1)
                     :transform #'(lambda (x) (max (- x 8) 13))
                     :transient t)

   (reserved :mysql-type (integer 10) :transient t :bind nil) ; reserved for future use.

   (auth-data :mysql-type (octets auth-data-length)
              :predicate (flagsp $mysql-capability-client-secure-connection capability-flags)
              ;; can we simplify the transform?
              :reduce #'(lambda (x y) (concatenate'(vector (unsigned-byte 8)) x y)))

   (auth-plugin :mysql-type (string :null-eof)
                :predicate (flagsp $mysql-capability-client-plugin-auth capability-flags))))

(defun process-initial-handshake-v10 (payload)
  ;; 1) Parse the payload
  (let ((packet (parse-initial-handshake-v10 payload)))
    ;; 2) Populate *mysql-connection* slots with data from initial handshake
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
          (initial-handshake-v10-packet-status-flags packet))
    ;; asedeno-TODO: Replace with an appropriate condition
    (assert (mysql-has-capability $mysql-capabilities-required))
    ;; asedeno-TODO: add optional logging/debugging functionality
    #+nil
    (format t "Auth Data: ~A~%~
               Auth Plugin: ~A~%~
               Server Capabilities: ~8,'0X~%~
               Client Capabilities: ~8,'0X~%~
               Combined Capabiltiies ~8,'0X~%~%"
            auth-data
            (babel:octets-to-string auth-plugin-name :encoding (mysql-connection-character-set *mysql-connection*))
            capability-flags
            $mysql-capabilities-supported
            (mysql-connection-capabilities *mysql-connection*))
    (values (initial-handshake-v10-packet-auth-data packet)
            (when (mysql-has-capability $mysql-capability-client-plugin-auth)
              ;; If we don't support pluggable-auth (and we don't
              ;; yet), don't bother returnin the plugin name.
              (initial-handshake-v10-packet-auth-plugin packet)))))

;;; asedeno-TODO: Write SSL upgrade path

#|
Protocol Handshake Response Packet (v41)

4 bytes        capability flags, CLIENT_PROTOCOL_41 always set
4 bytes        max-packet size
1 byte         character set
string[23]     reserved (all [0])
string[NUL]    username
if $mysql-capability-client-plugin-auth-lenenc-client-data
  lenenc-int   length of auth-response
  string[n]    auth-response
elif $mysql-capability-client-secure-connection
  1 byte       length of auth-response
  string[n]    auth-response
else
  string[NUL]  auth-response
endif
if $mysql-capability-client-connect-with-db
  string[NUL]  database
endif
if $mysql-capability-client-plugin-auth
  string[NUL]  auth plugin name
endif
if $mysql-capability-client-connect-attrs
  lenenc-int   length of all key-values
  ( lenenc-str   key
    lenenc-str   value
  ) [repeated]
endif
|#

(defun send-handshake-response-41 (&key username auth-plugin auth-response database)
  (declare (ignorable username auth-response database))
  (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
    (encode-fixed-length-integer s (mysql-connection-capabilities *mysql-connection*) 4)
    (encode-fixed-length-integer s #x1000000 4)
    (write-byte (mysql-connection-cs-coll *mysql-connection*) s)
    (encode-fixed-length-integer s 0 23) ; 23 reserved bytes
    (encode-null-terminated-string s (babel:string-to-octets
                                      username
                                      :encoding (mysql-connection-character-set *mysql-connection*)))
    (cond
      ((mysql-has-capability $mysql-capability-client-plugin-auth-lenec-client-data)
       (encode-length-encoded-integer s (length auth-response))
       (write-sequence auth-response s))
      ((mysql-has-capability $mysql-capability-client-secure-connection)
       (write-byte (length auth-response) s)
       (write-sequence auth-response s))
      (T
       (encode-null-terminated-string s auth-response)))
    ;; If the bit is still set at this point, then we have a database schema to specify.
    (when (mysql-has-capability $mysql-capability-client-connect-with-db)
      (encode-null-terminated-string s database))
    (when (mysql-has-capability $mysql-capability-client-plugin-auth)
      (encode-null-terminated-string s auth-plugin))
    #+mysql-client-connect-attributes
    (when (mysql-has-capability $mysql-capability-client-connect-attrs)
      ;; asedeno-TODO: When this is implemented, what sort of
      ;; attributes do we want to send? Are they hard-coded? Supplied
      ;; by the user? Both? Stored in the connection object?
      nil)
    (mysql-write-packet (flexi-streams:get-output-stream-sequence s))))

(defun process-initial-handshake-payload (payload)
  (ecase (aref payload 0)
    (10 (process-initial-handshake-v10 payload))))
