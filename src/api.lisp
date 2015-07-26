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
;;; Connection entry-point
(defun mysql-connect (&key
			(host "localhost")
			(port 3306)
			(username "")
			(password "")
			database
                        (client-found-rows nil)
			(compress nil)
			(ssl :unspecified)
			ssl-verify)
  "Connect to a MySQL over a network (AF_INET) socket and begin the MySQL Handshake.
   Returns a QMYND:MYSQL-CONNECTION object or signals a QMYND:MYSQL-ERROR.
   Accepts the following keyword arguments:
    HOST: The host to connect to. (default: \"localhost\")
    PORT: The port to connect to. (default: 3306)
    USERNAME: User to authenticate as.
    PASSWORD: Password to authenticate with.
    DATABASE: What database to use upon connecting. (default: nil)
    CLIENT-FOUND-ROWS: Whether or not to require the client-found-rows feature from the server (default: nil)
    COMPRESS: Whether or not to use compression. (default: nil).
    SSL: Whether or not to use SSL. (default: :UNSPECIFIED)
                     T - Forces SSL (or error out if it's not available).
                   NIL - Disable SSL, even if it is available.
          :UNSPECIFIED - Opportunistic use of SSL.
    SSL-VERIFY: Whether or not to verify the SSL certificate presented by the server. (Default: nil)"
  ;; Open Socket
  (let* ((socket (usocket:socket-connect host port
                                         :protocol :stream
                                         :element-type '(unsigned-byte 8)))
         (connection (make-instance 'mysql-inet-connection
                                    :socket socket
                                    :stream (usocket:socket-stream socket)
                                    :default-schema database)))

    (mysql-connect-do-handshake connection username password database
                                :client-found-rows client-found-rows
                                :compress compress
                                :ssl ssl
                                :ssl-verify ssl-verify)))

;;; AF_LOCAL sockets should really be folded into usocket. For now, just implement CCL and SBCL support.
#+(or ccl sbcl ecl)
(defun mysql-local-connect (&key (path #P"/var/run/mysqld/mysqld.sock")
                              (username "")
                              (password "")
                              database
                              (client-found-rows nil))
  "Connect to a MySQL over a local (AF_LOCAL) socket and begin the MySQL Handshake.
   Returns a QMYND:MYSQL-CONNECTION object or signals a QMYND:MYSQL-ERROR.
   Accepts the following keyword arguments:
    PATH: The path of the local socket to connect to. (default: #P\"/var/run/mysqld/mysqld.sock\")
    USERNAME: User to authenticate as.
    PASSWORD: Password to authenticate with.
    DATABASE: What database to use upon connecting. (default: nil)
    CLIENT-FOUND-ROWS: Whether or not to require the client-found-rows feature from the server (default: nil)"
  ;; Open Socket
  (let* ((socket
           #+ccl (ccl:make-socket :address-family :file
                                  :connect :active
                                  :remote-filename path)
           #+(or sbcl ecl)
           (let ((socket (make-instance 'sb-bsd-sockets:local-socket
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
                                    #+(or sbcl ecl)
                                    (sb-bsd-sockets:socket-make-stream
                                     socket :input t
                                     :output t
                                     :element-type '(unsigned-byte 8))
                                    :default-schema database)))
    (mysql-connect-do-handshake connection username password database
                                :client-found-rows client-found-rows
                                :ssl nil)))

(defmethod mysql-disconnect ((c mysql-base-connection))
  "Shut down a MySQL connection."
  (when (mysql-connection-connected c)
    (with-mysql-connection (c)
      (send-command-quit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple SQL Queries
(defun mysql-query (connection query-string
		    &key row-fn (as-text nil) (result-type 'vector))
  "Send a SQL Query over the connection using the MySQL Text Protocol.
   For queries that return rows, returns two values:
    A vector of rows, each of which is a vector of columns.
    A vector of column descriptors.
   For queries that don't return rows, returns a QMYND:RESPONSE-OK-PACKET.
   May signal a QMYND:MYSQL-ERROR."
  (with-mysql-connection (connection)
    (send-command-query query-string
			:row-fn row-fn
			:as-text as-text
			:result-type result-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prepared Statements
(defun mysql-statement-prepare (connection query-string)
  "Prepares a SQL Query using the MySQL Prepared Statement Protocol.
   Returns a QMYND:MYSQL-PREPARED-STATEMENT.
   NB: The QMYND:MYSQL-PREPARED-STATEMENT remembers the
       QMYND:MYSQL-CONNECTION it was prepared for."
  (with-mysql-connection (connection)
    (send-command-statement-prepare query-string)))

(defmethod mysql-statement-execute ((statement mysql-prepared-statement) &key parameters)
  "Executes a QMYND:MYSQL-PREPARED-STATEMENT.
   Accepts the following keyword arguments:
    PARAMETERS: a sequence of parameters for the placeholders in the prepared statement, if any."
  (let ((connection (mysql-prepared-statement-connection statement)))
    (unless connection (error 'invalid-prepared-statement))
    (with-mysql-connection (connection)
      (send-command-statement-execute statement :parameters parameters))))

(defmethod mysql-statement-close ((statement mysql-prepared-statement))
  "Deallocates and invalidates STATEMENT."
  (let ((connection (mysql-prepared-statement-connection statement)))
    (unless connection (error 'invalid-prepared-statement))
    (with-mysql-connection (connection)
      (send-command-statement-close statement)
      (mysql-connection-remove-stale-prepared-statements *mysql-connection*)
      (values))))
