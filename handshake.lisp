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

#|
Protocol Handshake Packet (v10)

1 byte         [0a] protocol version
string[NUL]    server version
4 bytes        connection id
string[8]      auth-plugin-data-part-1
1 byte         [00] filler
2 bytes        capability flags

if the packet continues:
  1 byte       character set
  2 bytes      status flags
  2 bytes      capability flags [upper bits]

  if $mysql-capability-client-plugin-auth
    1 byte     length of auth-plugin-data
  else
    1 byte     [00]
  endif
  string[10]   reserved, all [00]
  if $mysql-capability-client-secure-connection
    string[len]  auth-plugin-data-part-2; len = max(13, (- length of auth-plugin-data 8))
  endif
  if $mysql-capability-client-plugin-auth
    if (or (and (>= version 5.5.7) (< version 5.5.10))
           (and (>= version 5.6.0) (< version 5.6.2)))
      string[EOF] auth-plugin name
    elif (or (>= version 5.5.10) (>= version 5.6.2))
      string[NUL] auth-plugin name
    endif
  endif
endif
|#

(defun process-initial-handshake-v10-payload (payload)
  ;; 1) Parse the payload
  (let* ((s (flexi-streams:make-flexi-stream
             (flexi-streams:make-in-memory-input-stream payload :start 1)))
         (server-version (parse-null-terminated-string s))
         (connection-id (parse-fixed-length-integer s 4))
         (auth-data (parse-fixed-length-string s 8))
         (capability-flags (progn (assert (zerop (read-byte s)))
                                  (parse-fixed-length-integer s 2)))
         character-set
         status-flags
         auth-plugin-name)

    ;; 1a) Keep parsing the packet if there's more
    (when (flexi-streams:peek-byte s nil nil)
      (setf character-set (read-byte s)
            status-flags (parse-fixed-length-integer s 2)
            capability-flags (logior capability-flags
                                     (ash (parse-fixed-length-integer s 2) 16)))

      (let ((auth-data-length (read-byte s)))
        (unless (flagsp $mysql-capability-client-plugin-auth capability-flags)
          (assert (zerop auth-data-length)))

        ;; Reserved bytes
        (assert (every #'zerop (parse-fixed-length-string s 10)))
        ;; More auth data
        (when (flagsp $mysql-capability-client-secure-connection capability-flags)
          (setf auth-data
                (concatenate '(vector (unsigned-byte 8))
                             auth-data
                             (parse-fixed-length-string s (max 13 (- auth-data-length 8))))))
        ;; Auth plugin name
        (when (flagsp $mysql-capability-client-plugin-auth capability-flags)
          ;; eof-error-p is nil to account for http://bugs.mysql.com/bug.php?id=59453
          (setf auth-plugin-name (parse-null-terminated-string s nil)))))

    ;; 2) Populate *mysql-connection* slots with data from initial handshake
    (setf (mysql-connection-server-version *mysql-connection*)
          (babel:octets-to-string server-version :encoding :utf-8)

          (mysql-connection-connection-id  *mysql-connection*)
          connection-id

          (mysql-connection-capabilities *mysql-connection*)
          (logand (mysql-connection-capabilities *mysql-connection*)
                  capability-flags)

          (mysql-connection-character-set *mysql-connection*)
          (mysql-cs-coll-to-character-encoding character-set)

          (mysql-connection-cs-coll *mysql-connection*)
          character-set

          (mysql-connection-status-flags *mysql-connection*)
          status-flags)
    ;;; asedeno-TODO: Replace with an appropriate condition
    (assert (mysql-has-capability $mysql-capabilities-required))
    ;;; asedeno-TODO: add optional logging/debugging functionality
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
    (values auth-data
            (when (mysql-has-capability $mysql-capability-client-plugin-auth)
              ;; If we don't support pluggable-auth (and we don't
              ;; yet), don't bother returnin the plugin name.
              auth-plugin-name))))

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
      ;;; asedeno-TODO: When this is implemented, what sort of
      ;;; attributes do we want to send? Are they hard-coded? Supplied
      ;;; by the user? Both? Stored in the connection object?
      nil)
    (mysql-write-packet (flexi-streams:get-output-stream-sequence s))))

(defun process-initial-handshake-payload (payload)
  (ecase (aref payload 0)
    (10 (process-initial-handshake-v10-payload payload))))
