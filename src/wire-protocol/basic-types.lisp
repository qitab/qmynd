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
;;; asedeno-TODO: turn up compiler optimizations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.1.1.1. Integer

;;; 15.1.1.1.1. fixed length integer
;;; Little endian fixed-length integers with lengths (1 2 3 4 6 8)

(defun read-fixed-length-integer (length stream &key signed)
  "Read LENGTH bytes from STREAM as an integer (little endian).
   Accepts the following keyword arguments:
    SIGNED - treat the integer as signed."
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
  "Write INT to STREAM as a LENGTH byte integer."
  (loop
    repeat length
    for i fixnum from 0 by 8
    do (write-byte (ldb (byte 8 i) int) stream)))

;;; 15.1.1.1.2. length encoded integer

(defun read-length-encoded-integer (stream &key null-ok)
  "Read a MySQL Length-Encoded Integer from STREAM.
   Accepts the following keyword arguments:
    NULL-OK - Parse #xFB as NULL.
   Signals an error when we fail to parse an integer."
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
      ;; asedeno-TODO: signal a useful condition here.
      (t (error "bad length-encoded-integer")))))

(defun write-length-encoded-integer (int stream)
  "Write INT to STREAM as a MySQL Length-Encoded Integer.
   Assumes INT is non-negative.
   Signals an error if INT is too big."
  (assert (not (minusp int)))
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
    ;; asedeno-TODO: signal a useful condition here.
    (t (error "invalid input to write-length-encoded-integer"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.1.1.2. String - A sequence of bytes (aka octets).

;;; NB: No character encoding/decoding is performed at this stage.

;;; Protocol::FixedLengthString
;;; A string with a known length

(defun read-fixed-length-string (length stream)
  "Read LENGTH bytes from STREAM, returns them in a vector.
   NB: MySQL calls this a string, but we treat it as a vector of octets."
  (let ((octets (make-array length :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
    (read-sequence octets stream)
    octets))

;; Just use write-sequence directly
;; (defun write-string (octets stream)
;;   (write-sequence octets stream))

;;; Protocol::NulTerminatedString
;;; A string terminated by a NUL byte.

(defgeneric read-null-terminated-string (stream &optional eof-error-p)
  (:documentation
  "Read bytes from STREAM until we find a NUL byte.
   Accepts the following keyword arguments:
    EOF-ERROR-P - if NIL, treat EOF as NUL. (default: T)"))

(defmethod read-null-terminated-string (stream &optional (eof-error-p t)
                                        &aux (length 16))
  (let ((octets (make-array length :element-type '(unsigned-byte 8)
                                   :initial-element 0
                                   :adjustable t)))
    (loop
      for i fixnum from 0
      as b fixnum = (read-byte stream eof-error-p (unless eof-error-p 0))
      unless (< i length) do
        (incf length length)
        (setf octets (adjust-array octets length))
      when (= b 0)
        return (adjust-array octets i)
      do (setf (aref octets i) b))))

(defmethod read-null-terminated-string ((stream flexi-streams::vector-input-stream)
                                        &optional (eof-error-p t))
  ;; We're going to break the flexi-streams abstraction here to get an
  ;; exact length.
  (with-accessors ((index flexi-streams::vector-stream-index)
                   (end flexi-streams::vector-stream-end)
                   (vector flexi-streams::vector-stream-vector))
      stream
    (let ((position (position 0 vector :start index)))
      (cond
        ((and eof-error-p (null position))
         ;; Consume the stream and signal EOF
         (setf index end)
         (error 'end-of-file :stream stream))
        (t
         (let ((octets (make-array (- (or position end) index) :element-type '(unsigned-byte 8)
                                                               :initial-element 0)))
           (read-sequence octets stream)
           (when position (incf index)) ; Consume the NUL byte if necessary.
           octets))))))

(defun write-null-terminated-string (octets stream)
  "Write OCTETS to STREAM followed by a NUL byte.
   Assumes no NUL bytes exist in OCTETS."
  (assert (notany #'zerop octets))
  (write-sequence octets stream)
  (write-byte 0 stream))

;;; Protocol::VariableLengthString
;;; A string with a length determine by another field
;;; This will be implemented at a higher level using fixed-length-strings
;;; and knowledge of the other field.

;;; Protocol::LengthEncodedString
;;; A string prefixed by its length as a length-encoded integer

(defun read-length-encoded-string (stream &key null-ok)
  "Read a MySQL Length-Encoded Intgeer from STREAM, then read that many bytes from STREAM.
   Accepts the following keyword arguments:
    NULL-OK: Allow READ-LENGTH-ENCODED-INTEGER to treat #xFB as NULL."
  (let ((length (read-length-encoded-integer stream :null-ok null-ok)))
    (when length
      (read-fixed-length-string length stream))))

(defun write-length-encoded-string (octets stream)
  "Write the length of OCTETS to STREAM as a MySQL Length-Encoded Integer,
   then write OCTETS to STREAM."
  (let ((length (length octets)))
    (write-length-encoded-integer length stream)
    (write-sequence octets stream)))

;;; Protocol::RestOfPacketString
;;; Just read the rest of the packet

(defgeneric read-rest-of-packet-string (stream)
  (:documentation
   "Returns the rest of a stream as an array."))

(defmethod read-rest-of-packet-string (stream &aux (length 16))
  (let ((octets (make-array length :element-type '(unsigned-byte 8)
                                   :initial-element 0
                                   :adjustable t)))
    (loop
      for i fixnum from 0
      as b fixnum = (read-byte stream nil -1)
      unless (< i length) do
        (incf length length)
        (setf octets (adjust-array octets length))
      when (= b -1)
        return (adjust-array octets i)
      do (setf (aref octets i) b))))

(defmethod read-rest-of-packet-string ((stream flexi-streams::vector-input-stream))
  ;; We're going to break the flexi-streams abstraction here to get an
  ;; exact length.
  (with-accessors ((index flexi-streams::vector-stream-index)
                   (end flexi-streams::vector-stream-end)
                   (vector flexi-streams::vector-stream-vector))
      stream
    (let ((octets (make-array (- end index) :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
      (read-sequence octets stream)
      octets)))
