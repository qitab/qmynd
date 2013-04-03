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

;;; AF_LOCAL sockets should really be folded into usocket. For now, just implement CCL and SBCL support.
#+(or ccl sbcl)
(defun mysql-local-connect (&key (path #P"/var/run/mysqld/mysqld.sock") (username "") (password "") database)
  ;; Open Socket
  (let* ((socket
           #+ccl (ccl:make-socket :address-family :file
                                  :connect :active
                                  :remote-filename path)
           #+sbcl (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                                               :type :stream)))
                    (sb-bsd-sockets:socket-connect socket (etypecase path
                                                            (pathname (namestring path))
                                                            (string path)))
                    socket)
           )
         (connection (make-instance 'mysql-local-connection
                                    :socket socket
                                    :stream
                                    #+ccl socket
                                    #+sbcl (sb-bsd-sockets:socket-make-stream
                                            socket :input t
                                                   :output t
                                                   :element-type '(unsigned-byte 8))
                                    :default-schema database)))
    (mysql-connect-do-handshake connection username password database :ssl nil)))

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

(defmethod mysql-statement-close ((statement mysql-prepared-statement))
  (let ((connection (mysql-prepared-statement-connection statement)))
    (unless connection (error 'invalid-prepared-statement))
    (with-mysql-connection (connection)
      (send-command-statement-close statement)
      (mysql-connection-remove-stale-prepared-statements *mysql-connection*)
      (values))))
