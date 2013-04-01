;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
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

;;; asedeno-TODO: Write SSL upgrade path

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
  (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
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
      nil)
    (mysql-write-packet (flexi-streams:get-output-stream-sequence s))))

(defun process-initial-handshake-payload (payload)
  (ecase (aref payload 0)
    (10 (process-initial-handshake-v10 payload))))
