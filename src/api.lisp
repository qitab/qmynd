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
(defun mysql-connect (&key (host "localhost") (port 3306) (username "") (password "") database (ssl :unspecified) ssl-verify)
  ;; Open Socket
  (let* ((socket (usocket:socket-connect host port
                                         :protocol :stream
                                         :element-type '(unsigned-byte 8)))
         (connection (make-instance 'mysql-inet-connection
                                    :socket socket
                                    :stream (usocket:socket-stream socket)
                                    :default-schema database)))
    (mysql-connect-do-handshake connection username password database
                                :ssl ssl
                                :ssl-verify ssl-verify)))

;;; AF_LOCAL sockets should really be folded into usocket. For now, just implement CCL support.
#+(or ccl)
(defun mysql-local-connect (&key (path #P"/var/run/mysqld/mysqld.sock") (username "") (password "") database)
  ;; Open Socket
  (let* ((socket
           #+ccl (ccl:make-socket :address-family :file
                                  :connect :active
                                  :remote-filename path)
           )
         (connection (make-instance 'mysql-local-connection
                                    :socket socket
                                    :stream
                                    #+ccl socket
                                    :default-schema database)))
    (mysql-connect-do-handshake connection username password database :ssl nil)))

(defun mysql-connect-do-handshake (connection username password database &key ssl ssl-verify)
  ;; Read a wire packet
  (let ((initial-handshake-payload (mysql-connection-read-packet connection)))
    (with-mysql-connection (connection)
      ;; Process Initial Handshake
      (process-initial-handshake-payload initial-handshake-payload)

      (unless database
        (setf (mysql-connection-capabilities connection)
              (logandc2 (mysql-connection-capabilities connection)
                        +mysql-capability-client-connect-with-db+)))

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

      (when (mysql-has-capability +mysql-capability-client-compress+)
        (setf (mysql-connection-stream connection)
              (make-instance 'mysql-compressed-stream
                             :stream (mysql-connection-stream connection)))))

    (setf (mysql-connection-connected connection) t)
    connection))


(defmethod mysql-disconnect ((c mysql-base-connection))
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
