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
;;; Conncetion entry-point
(defun mysql-connect (&key (host "localhost") (port 3306) (username "") (password "") database)
  ;; Open Socket
  (let* ((socket (usocket:socket-connect host port
                                         :protocol :stream
                                         :element-type '(unsigned-byte 8)))
         (connection (make-instance 'mysql-connection
                                    :socket socket
                                    :stream (usocket:socket-stream socket)
                                    :default-schema database))
         ;; Read a wire packet
         (initial-handshake-payload (mysql-connection-read-packet connection)))
    (with-mysql-connection (connection)
      ;; Process Initial Handshake
      (process-initial-handshake-payload initial-handshake-payload)

      (unless database
        (setf (mysql-connection-capabilities connection)
              (logandc2 (mysql-connection-capabilities connection)
                        +mysql-capability-client-connect-with-db+)))

      (when (mysql-has-capability +mysql-capability-client-ssl+)
        (send-ssl-request-packet))

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
          (usocket:socket-close socket)
          (error e))))

    (setf (mysql-connection-connected connection) t)
    connection))

(defmethod mysql-disconnect ((c mysql-connection))
  (when (mysql-connection-connected c)
    (with-mysql-connection (c)
      (send-command-quit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple SQL Queries
(defun mysql-query (connection query-string)
  (with-mysql-connection (connection)
    (send-command-query query-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepared Statements
(defun mysql-statement-prepare (connection query-string)
  (with-mysql-connection (connection)
    (send-command-statement-prepare query-string)))

(defmethod mysql-statement-execute ((statement mysql-prepared-statement) &key parameters)
  (let ((connection (mysql-prepared-statement-connection statement)))
    (unless connection (error 'invalid-prepared-statement))
    (with-mysql-connection (connection)
      (send-command-statement-execute statement :parameters parameters))))

(defmethod mysql-statement-reset ((statement mysql-prepared-statement))
  (let ((connection (mysql-prepared-statement-connection statement)))
    (unless connection (error 'invalid-prepared-statement))
    (with-mysql-connection (connection)
      (send-command-statement-reset statement)
      (values))))

(defmethod mysql-statement-close ((statement mysql-prepared-statement))
  (let ((connection (mysql-prepared-statement-connection statement)))
    (unless connection (error 'invalid-prepared-statement))
    (with-mysql-connection (connection)
      (send-command-statement-close statement)
      (mysql-connection-remove-stale-prepared-statements *mysql-connection*)
      (values))))
