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

;;; 15.1.1.2. String - A sequence of bytes (aka octets).

;;; NB: No character encoding/decoding is performed at this stage.

;;; Protocol::FixedLengthString
;;; A string with a known length

(defun read-fixed-length-string (length stream)
  (let ((octets (make-sequence '(vector (unsigned-byte 8)) length)))
    (read-sequence octets stream)
    octets))

;; Just use write-sequence directly
;; (defun write-string (octets stream)
;;   (write-sequence octets stream))

;;; Protocol::NulTerminatedString
;;; A string terminated by a NUL byte.

(defun read-null-terminated-string (stream &optional (eof-error-p t))
  (let* ((length 16)
         (octets (make-array length
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :initial-element 0)))
    (loop
      for i fixnum from 0
      as b fixnum = (read-byte stream eof-error-p (unless eof-error-p 0))
      unless (< i length) do
        (incf length length) and do
          (adjust-array octets length)
      when (= b 0) do
        (adjust-array octets i) and return octets
      do (setf (aref octets i) b))))

(defun write-null-terminated-string (octets stream)
  (assert (notany #'zerop octets))
  (write-sequence octets stream)
  (write-byte 0 stream))

;;; Protocol::VariableLengthString
;;; A string with a length determine by another field
;;; This will be implemented at a higher level using fixed-length-strings and knowledge of the other field.

;;; Protocol::LengthEncodedString
;;; A string prefixed by its length as a length-encoded integer

(defun read-length-encoded-string (stream)
  (read-fixed-length-string (read-length-encoded-integer stream)
                            stream))

(defun write-length-encoded-string (octets stream)
  (let ((length (length octets)))
    (write-length-encoded-integer length stream)
    (write-sequence octets stream)))

;;; Protocol::RestOfPacketString
;;; Just read the rest of the packet

(defgeneric read-rest-of-packet-string (stream)
  (:documentation
   "Returns the rest of a stream as an array. Only implemented on
   flexi-streams::vector-input-stream."))

(defmethod read-rest-of-packet-string ((stream flexi-streams::vector-input-stream))
  ;; We're going to break the flexi-streams abstraction here to get an
  ;; exact length.
  (let* ((length (- (flexi-streams::vector-stream-end stream)
                    (flexi-streams::vector-stream-index stream)))
         (octets (make-array length
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (read-sequence octets stream)
    octets))
