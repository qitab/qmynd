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

(defvar *mysql-connection* nil
  "All API entry-points after connect take a mysql-connection argument
  and must bind *mysql-connection* to that connection for internal
  function use.")

(defmacro with-mysql-connection ((c) &body body)
  `(let* ((*mysql-connection* ,c)
          (babel::*default-character-encoding* (mysql-connection-character-set *mysql-connection*)))
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
                        :accessor mysql-connection-prepared-statements)))

(defclass mysql-inet-connection (mysql-base-connection)
  ((socket :type (or usocket:stream-usocket null)
           :initarg :socket
           :accessor mysql-connection-socket)))

(defmethod mysql-connection-close-socket ((c mysql-inet-connection))
  (usocket:socket-close (mysql-connection-socket c))
  (setf (mysql-connection-connected *mysql-connection*) nil))

#+(or ccl)
(progn

  (defclass mysql-local-connection (mysql-base-connection)
    ((socket :initarg :socket
             :accessor mysql-connection-socket)))

  (defmethod mysql-connection-close-socket ((c mysql-local-connection))
    (let ((socket (mysql-connection-socket c)))
      #+ccl (ccl::close (mysql-connection-socket c))
      )
    (setf (mysql-connection-connected *mysql-connection*) nil))

) ; progn

(defmethod mysql-connection-remove-stale-prepared-statements ((c mysql-base-connection))
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


;;; Packet utilities
(defmethod mysql-connection-write-packet ((c mysql-base-connection) payload)
  (setf (mysql-connection-sequence-id c)
        (write-wire-packet (mysql-connection-stream c)
                           payload
                           :sequence-id (mysql-connection-sequence-id c)))
  (values))

(defmethod mysql-connection-read-packet ((c mysql-base-connection))
  (multiple-value-bind (payload seq-id)
      (read-wire-packet (mysql-connection-stream c)
                        :expected-sequence-id (mysql-connection-sequence-id c))
    (setf (mysql-connection-sequence-id c) seq-id)
    payload))

(defmethod mysql-connection-command-init ((c mysql-base-connection) command)
  (let ((stream (mysql-connection-stream c)))
    (when (typep stream 'mysql-compressed-stream)
      (setf (mysql-compressed-stream-sequence-id stream) 0)))
  (setf (mysql-connection-sequence-id c) 0
        (mysql-connection-current-command c) command))

(defun mysql-command-init (command)
  (mysql-connection-command-init *mysql-connection* command))

(defun mysql-current-command-p (command)
  (eq (mysql-connection-current-command *mysql-connection*) command))

(defun mysql-write-packet (payload)
  (mysql-connection-write-packet *mysql-connection* payload))

(defun mysql-read-packet ()
  (mysql-connection-read-packet *mysql-connection*))
