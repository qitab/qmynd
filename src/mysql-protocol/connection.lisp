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
                    :accessor mysql-connection-sequence-id)
   (default-schema  :type (or string null)
                    :initarg :default-schema
                    :accessor mysql-connection-default-schema)))

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

(defmethod mysql-command-init ((c mysql-connection))
  (setf (mysql-connection-sequence-id c) 0))

(defun mysql-write-packet (payload)
  (mysql-connection-write-packet *mysql-connection* payload))

(defun mysql-read-packet ()
  (mysql-connection-read-packet *mysql-connection*))
