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

;;; Conncetion entry-point
(defun mysql-connect (&key (host "localhost") (port 3306) (username "") (password "") database)
  ;; 1) Open Socket
  (let* ((socket (usocket:socket-connect host port
                                         :protocol :stream
                                         :element-type '(unsigned-byte 8)))
         (connection (make-instance 'mysql-connection
                                    :socket socket
                                    :default-schema database))
         ;; 2) Read a wire packet
         (initial-handshake-payload (mysql-connection-read-packet connection)))
    (bind-mysql-connection (connection)
      ;; 3) Process Initial Handshake
      (multiple-value-bind (auth-data auth-plugin)
          (process-initial-handshake-payload initial-handshake-payload)

        ;; asedeno-TODO: Negotiate SSL

        (unless database
          (setf (mysql-connection-capabilities connection)
                (logxor (mysql-connection-capabilities connection)
                        +mysql-capability-client-connect-with-db+)))

        ;; 4) Prepare Auth Response
        (let ((auth-response (generate-auth-response password auth-data auth-plugin)))
          ;; 5) Prepare Initial Response OR Close and Signal
          (send-handshake-response-41 :username username :auth-response auth-response :auth-plugin auth-plugin :database database)
          (let ((response (parse-response (mysql-read-packet))))
            (assert (typep
                     response
                     'response-ok-packet))))))
    (setf (mysql-connection-connected connection) t)
    connection))

(defmethod mysql-disconnect ((c mysql-connection))
  (when (mysql-connection-connected c)
    (bind-mysql-connection (c)
      (send-command-quit))))
