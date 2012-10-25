;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd)
;;; asedeno-TODO: turn up compiler optimizations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.1.1.1. Integer

;;; 15.1.1.1.1. fixed length integer
;;; Little endian fixed-length integers with lengths (1 2 3 4 6 8)

(defun read-fixed-length-integer (length stream &key signed)
  (let ((result 0)
        negative)
    (loop
      repeat length
      for i fixnum from 0 by 8
      for byte = (read-byte stream) then (read-byte stream)
      do (setf (ldb (byte 8 i) result) byte)
      finally (setf negative (plusp (ash byte -7))))
    (if (and signed negative)
        (logxor (lognot result) (1- (ash 1 (* 8 length))))
        result)))

(defun write-fixed-length-integer (int length stream)
  (loop
    repeat length
    for i fixnum from 0 by 8
    do (write-byte (ldb (byte 8 i) int) stream)))

;;; 15.1.1.1.2. length encoded integer

(defun read-length-encoded-integer (stream &key null-ok)
  (let ((n (read-byte stream)))
    (cond
      ((< n #xfb) n)
      ;; #xfb here is undefined, though it may mean NULL in certain contexts.
      ((and null-ok (= n #xfb))
       nil)
      ((= n #xfc)
       (read-fixed-length-integer 2 stream))
      ((= n #xfd)
       (read-fixed-length-integer 3 stream))
      ((= n #xfe)
       (read-fixed-length-integer 8 stream))
      ;; #xff here is undefined, though it may be an error packet in certain contexts.
      (t (error "bad length-encoded-integer")))))

(defun write-length-encoded-integer (int stream)
  (cond
    ((< int 251)
     (write-byte int stream))
    ((< int #x10000)
     (write-byte #xfc stream)
     (write-fixed-length-integer int 2 stream))
    ((< int #x1000000)
     (write-byte #xfd stream)
     (write-fixed-length-integer int 3 stream))
    ((< int #x10000000000000000)
     (write-byte #xfe stream)
     (write-fixed-length-integer int 8 stream))
    (t (error "invalid input to write-length-encoded-integer"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun read-length-encoded-string (stream &key null-ok)
  (let ((length (read-length-encoded-integer stream :null-ok null-ok)))
    (when length
      (read-fixed-length-string length stream))))

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
