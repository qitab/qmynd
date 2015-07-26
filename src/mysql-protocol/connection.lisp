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

(defvar *mysql-connection* nil
  "All API entry-points after connect take a mysql-connection argument
  and must bind *mysql-connection* to that connection for internal
  function use.")

(defvar *mysql-encoding* nil
  "As it happens, it's quite easy to have data in an encoding known only by
   the application, and completely unknown by MySQL which will keep sending
   unrelated meta-data. Use this setting to force the Qmynd driver to be
   using the encoding you know your data is encoded with.")

(defmacro with-mysql-connection ((c) &body body)
  `(let* ((*mysql-connection* ,c)
          (babel::*default-character-encoding*
           (mysql-connection-character-set *mysql-connection*)))
     ,@body))

(defclass mysql-base-connection ()
  ((connected       :type boolean
                    :accessor mysql-connection-connected)
   (stream          :initarg :stream
                    :accessor mysql-connection-stream)
   (server-version  :type (or string null)
                    :accessor mysql-connection-server-version)
   (connection-id   :type (or integer null)
                    :accessor mysql-connection-connection-id)
   (capabilities    :type integer
                    :accessor mysql-connection-capabilities
                    :initform (mysql-capabilities-supported))
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
                    :accessor mysql-connection-sequence-id)
   (auth-data       :type (vector (unsigned-byte 8))
                    :accessor mysql-connection-auth-data)
   (auth-plugin     :type (or string null)
                    :accessor mysql-connection-auth-plugin)
   (default-schema  :type (or string null)
                    :initarg :default-schema
                    :accessor mysql-connection-default-schema)
   (current-command :type (or integer null)
                    :initform nil
                    :accessor mysql-connection-current-command)
   ;; This is internal library state. It may be destructively modified by the library.
   (prepared-statements :type list
                        :initform nil
                        :accessor mysql-connection-prepared-statements))
  (:documentation "The base class for all MySQL connections."))

(defgeneric mysql-connection-close-socket (connection)
  (:documentation "Close the MySQL connection's socket."))

(defclass mysql-inet-connection (mysql-base-connection)
  ((socket :type (or usocket:stream-usocket null)
           :initarg :socket
           :accessor mysql-connection-socket))
  (:documentation "An AF_INET MySQL connections."))

(defmethod mysql-connection-close-socket ((c mysql-inet-connection))
  (usocket:socket-close (mysql-connection-socket c))
  (setf (mysql-connection-connected *mysql-connection*) nil))

#+(or ccl sbcl ecl)
(progn

  (defclass mysql-local-connection (mysql-base-connection)
    ((socket :initarg :socket
             :accessor mysql-connection-socket))
  (:documentation "An AF_LOCAL MySQL connection."))

  (defmethod mysql-connection-close-socket ((c mysql-local-connection))
    (let ((socket (mysql-connection-socket c)))
      #+ccl (ccl::close socket)
      #+(or sbcl ecl)
      (sb-bsd-sockets:socket-close socket)
      )
    (setf (mysql-connection-connected *mysql-connection*) nil))

) ; progn

(defmethod mysql-connection-remove-stale-prepared-statements ((c mysql-base-connection))
  "Removes from C all prepared statements that do not have C as their connection."
  (setf (mysql-connection-prepared-statements *mysql-connection*)
        (delete-if-not
         #'(lambda (ps)
             (eq (mysql-prepared-statement-connection ps) *mysql-connection*))
         (mysql-connection-prepared-statements *mysql-connection*))))


;;; Flag utilities
(defun flagsp (bits-to-test bits-available &optional (mode :every))
  (ecase mode
    (:every  (= bits-to-test (logand bits-to-test bits-available)))
    (:some   (not (zerop (logand bits-to-test bits-available))))
    (:notany (zerop (logand bits-to-test bits-available)))))

(defmethod mysql-connection-has-status ((c mysql-base-connection) status-bits)
  (flagsp status-bits (mysql-connection-status-flags c)))

(defmethod mysql-connection-has-capability ((c mysql-base-connection) cap-bits)
  (flagsp cap-bits (mysql-connection-capabilities c)))

(defmethod mysql-connection-has-some-capability ((c mysql-base-connection) cap-bits)
  (flagsp cap-bits (mysql-connection-capabilities c) :some))

(defun mysql-has-capability (cap-bits)
  (mysql-connection-has-capability *mysql-connection* cap-bits))

(defun mysql-has-some-capability (cap-bits)
  (mysql-connection-has-some-capability *mysql-connection* cap-bits))

(defun mysql-add-required-capability (cap-bits)
  (setf (mysql-connection-capabilities *mysql-connection*)
        (logand (mysql-connection-capabilities *mysql-connection*) cap-bits)))

;;; Packet utilities
(defmethod mysql-connection-write-packet ((c mysql-base-connection) payload)
  "Write PAYLOAD to C's stream as a wire packet."
  (setf (mysql-connection-sequence-id c)
        (write-wire-packet (mysql-connection-stream c)
                           payload
                           :sequence-id (mysql-connection-sequence-id c)))
  (values))

(defmethod mysql-connection-read-packet ((c mysql-base-connection))
  "Read a wire packet from C's stream."
  (multiple-value-bind (stream seq-id)
      (read-wire-packet (mysql-connection-stream c)
                        :expected-sequence-id (mysql-connection-sequence-id c))
    (setf (mysql-connection-sequence-id c) seq-id)
    stream))

(defmethod mysql-connection-command-init ((c mysql-base-connection) command)
  "Initialize connection for a new command.
   Resets sequence-id in underlying stream(s)."
  (let ((stream (mysql-connection-stream c)))
    (when (typep stream 'mysql-compressed-stream)
      (setf (mysql-compressed-stream-sequence-id stream) 0)))
  (setf (mysql-connection-sequence-id c) 0
        (mysql-connection-current-command c) command))

(defun mysql-command-init (command)
  "Initialize the default MySQL connection for a new command.
   Resets sequence-id in underlying stream(s)."
  (mysql-connection-command-init *mysql-connection* command))

(defun mysql-current-command-p (command)
  "Tests to see if COMMAND is the current command of the default MySQL connection."
  (eq (mysql-connection-current-command *mysql-connection*) command))

(defun mysql-write-packet (payload)
  "Write PAYLOAD to the default MySQL connection's stream as a wire packet."
  (mysql-connection-write-packet *mysql-connection* payload))

(defun mysql-read-packet ()
  "Read a wire packet from the default MySQL connection's stream."
  (mysql-connection-read-packet *mysql-connection*))
