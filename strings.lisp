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

(defun parse-fixed-length-string (stream length)
  (let ((octets (make-sequence '(vector (unsigned-byte 8)) length)))
    (read-sequence octets stream)
    octets))

(defun encode-string (stream octets)
  (write-sequence octets stream))

;;; Protocol::NulTerminatedString
;;; A string terminated by a NUL byte.

(defun parse-null-terminated-string (stream)
  (let* ((length 16)
         (octets (make-array length
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :initial-element 0)))
    (loop
       for i fixnum from 0
       as b fixnum = (read-byte stream)
       unless (< i length) do
         (incf length length) and do
         (adjust-array octets length)
       when (= b 0) return octets
       do (setf (aref octets i) b))))

(defun encode-null-terminated-string (stream octets)
  (assert (notany (lambda (ch) (char= ch #\NUL)) octets))
  (encode-string stream octets)
  (write-byte 0 stream))

;;; Protocol::VariableLengthString
;;; A string with a length determine by another field
;;; This will be implemented at a higher level using fixed-length-strings and knowledge of the other field.

;;; Protocol::LengthEncodedString
;;; A string prefixed by its length as a length-encoded integer

(defun parse-length-encoded-string (stream)
  (parse-fixed-length-string stream
                             (parse-length-encoded-integer stream)))

(defun encode-length-encoded-string (stream octets)
  (let ((length (length octets)))
    (encode-length-encoded-integer stream length)
    (write-sequence octets stream)))

;;; Protocol::RestOfPacketString
;;; Just read the rest of the packet

(defgeneric parse-rest-of-packet-string (stream)
  (:documentation
   "Returns the rest of a stream as an array. Only implemented on
   flexi-streams::vector-input-stream."))

(defmethod parse-rest-of-packet-string ((stream flexi-streams::vector-input-stream))
  ;; We're going to break the flexi-streams abstraction here to get an
  ;; exact length.
  (let* ((length (- (flexi-streams::vector-stream-end stream)
                    (flexi-streams::vector-stream-index stream)))
         (octets (make-array length
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :initial-element 0)))
    (read-sequence octets stream)
    octets))