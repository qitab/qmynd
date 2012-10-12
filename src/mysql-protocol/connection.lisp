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

(defvar *mysql-connection* nil
  "All API entry-points after connect take a mysql-connection argument
  and must bind *mysql-connection* to that connection for internal
  function use.")

(defmacro bind-mysql-connection ((c) &body body)
  `(let ((*mysql-connection* ,c))
     ,@body))

(defmacro with-mysql-connection ((c) &body body)
  `(let ((,c *mysql-connection*))
     ,@body))

(defclass mysql-connection ()
  ((connected       :type boolean
                    :accessor mysql-connection-connected)
   (socket          :type (or usocket:stream-usocket null)
                    :initarg :socket
                    :accessor mysql-connection-socket)
   (server-version  :type (or string null)
                    :accessor mysql-connection-server-version)
   (connection-id   :type (or integer null)
                    :accessor mysql-connection-connection-id)
   (capabilities    :type integer
                    :accessor mysql-connection-capabilities
                    :initform +mysql-capabilities-supported+)
   (character-set   :type keyword
                    :accessor mysql-connection-character-set
                    :initform :utf-8)   ;:iso-8859-1)
   (mysql-cs-coll   :type integer
                    :accessor mysql-connection-cs-coll
                    :initform +mysql-cs-coll-utf8-general-ci+)
   (status-flags    :type (or integer null)
                    :accessor mysql-connection-status-flags)
   (sequence-id     :type integer
                    :initform 0
                    :accessor mysql-connection-sequence-id)))

(defmethod mysql-connection-stream ((c mysql-connection))
  (usocket:socket-stream (mysql-connection-socket c)))


;;; Flag utilities
(defun flagsp (bits-to-test bits-available &optional (mode :every))
  (ecase mode
    (:every  (= bits-to-test (logand bits-to-test bits-available)))
    (:some   (not (zerop (logand bits-to-test bits-available))))
    (:notany (zerop (logand bits-to-test bits-available)))))

(defmethod mysql-connection-has-capability ((c mysql-connection) cap-bits)
  (flagsp cap-bits (mysql-connection-capabilities c)))

(defmethod mysql-connection-has-some-capability ((c mysql-connection) cap-bits)
  (flagsp cap-bits (mysql-connection-capabilities c) :some))

(defun mysql-has-capability (cap-bits)
  (mysql-connection-has-capability *mysql-connection* cap-bits))

(defun mysql-has-some-capability (cap-bits)
  (mysql-connection-has-some-capability *mysql-connection* cap-bits))


;;; Packet utilities
(defmethod mysql-connection-write-packet ((c mysql-connection) payload)
  (setf (mysql-connection-sequence-id c)
        (write-wire-packet (mysql-connection-stream c)
                           payload
                           :sequence-id (mysql-connection-sequence-id c)))
  (values))

(defmethod mysql-connection-read-packet ((c mysql-connection))
  (multiple-value-bind (payload seq-id)
      (read-wire-packet (mysql-connection-stream c)
                        :expected-sequence-id (mysql-connection-sequence-id c))
    (setf (mysql-connection-sequence-id c) seq-id)
    payload))

(defun mysql-write-packet (payload)
  (mysql-connection-write-packet *mysql-connection* payload))

(defun mysql-read-packet ()
  (mysql-connection-read-packet *mysql-connection*))

;;; Conncetion entry-point
(defun mysql-connect (&key (host "localhost") (port 3306) (username "") (password "") database)
  ;; 1) Open Socket
  (let* ((socket (usocket:socket-connect host port
                                         :protocol :stream
                                         :element-type '(unsigned-byte 8)))
         (connection (make-instance 'mysql-connection
                                    :socket socket))
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
    ;; asedeno-TODO: send a +mysql-command-quit+ here
    (usocket:socket-close (mysql-connection-socket c))
    (setf (mysql-connection-connected c) nil)))
