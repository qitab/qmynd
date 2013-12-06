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
(define-compiler-macro read-fixed-length-integer (&whole form bytes stream &rest keys)
  (case bytes
    (1 `(read-my-byte ,stream ,@keys))
    (2 `(read-2-bytes-integer ,stream ,@keys))
    (3 `(read-3-bytes-integer ,stream ,@keys))
    (4 `(read-4-bytes-integer ,stream ,@keys))
    (8 `(read-8-bytes-integer ,stream ,@keys))
    (10 `(read-10-bytes-integer ,stream ,@keys))
    (t form)))

(defun read-fixed-length-integer (length stream &key signed)
  "Read an integer of LENGTH bytes from STREAM."
  (ecase length
    (1 (read-my-byte stream))
    (2 (read-2-bytes-integer stream :signed signed))
    (3 (read-3-bytes-integer stream :signed signed))
    (4 (read-4-bytes-integer stream :signed signed))
    (6 (read-6-bytes-integer stream :signed signed))
    (8 (read-8-bytes-integer stream :signed signed))
    (10 (read-10-bytes-integer stream :signed signed))))

(declaim (inline unsigned-to-signed read-fixed-length-integer))

(defun unsigned-to-signed (byte n)
  (declare (type fixnum n) (type unsigned-byte byte))
  (logior byte (- (mask-field (byte 1 (1- (* n 8))) byte))))

(defun read-2-bytes-integer (stream &key signed)
  "Read 2 bytes from STREAM as an integer."
  (declare (type my-packet-stream stream))
  (let* ((byte-1    (read-my-byte stream))
         (byte-2    (read-my-byte stream))
         (unsigned  (logior (ash byte-2 8) byte-1)))
    (if signed (unsigned-to-signed unsigned 2) unsigned)))

(defun read-3-bytes-integer (stream  &key signed)
  (declare (type my-packet-stream stream))
  (let* ((byte-1    (read-my-byte stream))
         (byte-2    (read-my-byte stream))
         (byte-3    (read-my-byte stream))
         (unsigned
          (logior (ash byte-3 16) (ash byte-2  8) byte-1)))
    (if signed (unsigned-to-signed unsigned 3) unsigned)))

(defun read-4-bytes-integer (stream  &key signed)
  (declare (type my-packet-stream stream))
  (let* ((byte-1    (read-my-byte stream))
         (byte-2    (read-my-byte stream))
         (byte-3    (read-my-byte stream))
         (byte-4    (read-my-byte stream))
         (unsigned
          (logior (ash byte-4 24) (ash byte-3 16) (ash byte-2 8) byte-1)))
    (if signed (unsigned-to-signed unsigned 3) unsigned)))

(defun read-6-bytes-integer (stream  &key signed)
  (declare (type my-packet-stream stream))
  (let* ((byte-1    (read-my-byte stream))
         (byte-2    (read-my-byte stream))
         (byte-3    (read-my-byte stream))
         (byte-4    (read-my-byte stream))
         (byte-5    (read-my-byte stream))
         (byte-6    (read-my-byte stream))
         (unsigned
          (logior (ash byte-6 40) (ash byte-5 32) (ash byte-4 24)
                  (ash byte-3 16) (ash byte-2 8) byte-1)))
    (if signed (unsigned-to-signed unsigned 3) unsigned)))

(defun read-8-bytes-integer (stream &key signed)
  (declare (type my-packet-stream stream))
  (let ((unsigned
         (logior (read-4-bytes-integer stream)
                 (ash (read-4-bytes-integer stream) 32))))
    (if signed (unsigned-to-signed unsigned 8) unsigned)))

(defun read-10-bytes-integer (stream  &key signed)
  (declare (type my-packet-stream stream))
  (let* ((byte-1    (read-my-byte stream))
         (byte-2    (read-my-byte stream))
         (byte-3    (read-my-byte stream))
         (byte-4    (read-my-byte stream))
         (byte-5    (read-my-byte stream))
         (byte-6    (read-my-byte stream))
         (byte-7    (read-my-byte stream))
         (byte-8    (read-my-byte stream))
         (byte-9    (read-my-byte stream))
         (byte-10   (read-my-byte stream))
         (unsigned
          (logior (ash byte-10 72)
                  (ash byte-9 64) (ash byte-8 56) (ash byte-7 48)
                  (ash byte-6 40) (ash byte-5 32) (ash byte-4 24)
                  (ash byte-3 16) (ash byte-2  8) byte-1)))
    (if signed (unsigned-to-signed unsigned 10) unsigned)))

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
  (let ((n (read-my-byte stream)))
    (cond
      ((< n #xfb) n)
      ;; #xfb here is undefined, though it may mean NULL in certain contexts.
      ((and null-ok (= n #xfb))
       nil)
      ((= n #xfc)
       (read-2-bytes-integer stream))
      ((= n #xfd)
       (read-3-bytes-integer stream))
      ((= n #xfe)
       (read-8-bytes-integer stream))
      ;; #xff here is undefined, though it may be an error packet in certain contexts.
      (t (error (make-condition 'invalid-length-encoded-integer
                                :text "Bad length while reading a length-encoded integer."))))))

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
    (t (error (make-condition
               'invalid-length-encoded-integer
               :text (format nil "Integer ~A too large while length-encoding integer." int))))))

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
    (read-my-sequence octets stream)
    octets))

;; Just use write-sequence directly
;; (defun write-string (octets stream)
;;   (write-sequence octets stream))

;;; Protocol::NulTerminatedString
;;; A string terminated by a NUL byte.
(defun read-null-terminated-string (stream
                                    &optional (eof-error-p t)
                                    &aux (length 16))
  (declare (type my-packet-stream stream)
           (fixnum length))
  (let ((octets (make-array length :element-type '(unsigned-byte 8)
                                   :initial-element 0
                                   :adjustable t)))
    (loop
      for i fixnum from 0
      as b fixnum = (read-my-byte stream eof-error-p (unless eof-error-p 0))
      unless (< i length) do
        (incf length length)
        (setf octets (adjust-array octets length))
      when (= b 0)
        return (adjust-array octets i)
      do (setf (aref octets i) b))))

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
(defun read-rest-of-packet-string (stream)
  "Read the rest of the current MySQL packet."
  (declare (type my-packet-stream stream))
  (read-all-remaining-bytes stream))

