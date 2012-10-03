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

;;; 15.1.3. Generic Response Packets

#|
OK Packet

1 byte         [00] the OK header
lenenc-int     affected rows
lenenc-int     last-insert-id
if $mysql-capability-client-protocol-41
  2 bytes        status_flags
  2 bytes        warnings
elif $mysql-capability-client-transactions
  2 bytes        status_flags
endif
string[EOF]    info
|#

(defstruct mysql-response-ok-packet
  (affected-rows  nil :type integer)
  (last-insert-id nil :type integer)
  (status-flags   nil :type (or integer null))
  (warnings       nil :type (or integer null))
  (info           nil :type string))

(defun parse-ok-payload (payload)
  (let ((s (flexi-streams:make-in-memory-input-stream payload :start 1)))
    (make-mysql-response-ok-packet
     :affected-rows (parse-length-encoded-integer s)
     :last-insert-id (parse-length-encoded-integer s)
     :status-flags (when (mysql-has-some-capability
                          #.(logior $mysql-capability-client-protocol-41
                                    $mysql-capability-client-transactions))
                     (parse-fixed-length-integer s 2))
     :warnings (when (mysql-has-capability $mysql-capability-client-protocol-41)
                 (parse-fixed-length-integer s 2))
     :info (babel:octets-to-string (parse-rest-of-packet-string s)))))

#|
Error Packet

1 byte         [ff] the ERR header
2 bytes        error code
if $mysql-capability-client-protocol-41
  string[1]      '#' the sql-state marker
  string[5]      sql-state
endif
string[EOF]    error-message
|#

(defstruct mysql-response-error-packet
  (error-code    nil :type integer)
  (sql-state     nil :type (or string null))
  (error-message nil :type (or string null)))

(defun parse-error-payload (payload)
  (let ((s (flexi-streams:make-in-memory-input-stream payload :start 1)))
    (make-mysql-response-error-packet
     :error-code (parse-fixed-length-integer s 2)
     :sql-state (when (mysql-has-capability $mysql-capability-client-protocol-41)
                  (assert (= (read-byte s) #.(char-code #\#)))
                  (babel:octets-to-string  (parse-fixed-length-string s 5)))
     :error-message (babel:octets-to-string (parse-rest-of-packet-string s)))))
#|
End-of-File Packet

1 byte         [fe] the EOF header
if $mysql-capability-client-protocol-41
  2 bytes        warning count
  2 bytes        status flags
endif
|#
(defstruct mysql-response-end-of-file-packet
  (warning-count nil :type (or integer null))
  (status-flags  nil :type (or integer null)))

(defun parse-end-of-file-payload (payload)
  (let ((s (flexi-streams:make-in-memory-input-stream payload :start 1)))
    (make-mysql-response-end-of-file-packet
     :warning-count (when (mysql-has-capability $mysql-capability-client-protocol-41)
                      (parse-fixed-length-integer s 2))
     :status-flags (when (mysql-has-capability  $mysql-capability-client-protocol-41)
                     (parse-fixed-length-integer s 2)))))

(defun parse-response-payload (payload)
  (case (aref payload 0)
    (#x00 (parse-ok-payload payload))
    (#xfe (parse-end-of-file-payload payload))
    (#xff (parse-error-payload payload))))
