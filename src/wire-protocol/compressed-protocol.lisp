;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2013 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)

;;; 5.4. Compression

#|
A MySQL Compressed Packet consists of:
• 3 octets       - length of packet (compressed)
• 1 octet        - sequence id (reset with, but independent of the wire-packet sequence-id)
• 3 octets       - length of packet (uncompressed); 0 means payload was not compressed
• string[length] - the payload

The MySQL Compression Protocol is independent of the MySQL Wire Protocol
and is implemented here as a Gray stream that wraps the connection stream.

The wrapping occurs after the MySQL Handshake is complete if compression
is supported by the server and this library.

This functionality depends on the CHIPZ (decompression) and
SALZA2 (compression) libraries.

NB: A future version of this library may support the Compression protocol
using CHIPZ without SALZA, falling back on transmitting packets
uncompressed, but for now using the Compression protocol requires both.

|#

;;; Functions to read and write wire packets for the compressed protocol.
(defun read-compressed-wire-packet (stream &key (expected-sequence-id 0))
  "Read a compressed packet from STREAM."
  (let (payload
        (pos 0)
        (compressed-length (%read-3-octets stream))
        (sequence-id (if (= (read-byte stream) expected-sequence-id)
                         (setf expected-sequence-id (mod (1+ expected-sequence-id) 256))
                         (error (make-instance 'unexpected-sequence-id))))
        (decompressed-length (%read-3-octets stream)))
    (assert (plusp compressed-length))
    (setf payload (make-array compressed-length :element-type '(unsigned-byte 8)))
    (loop do (setf pos (read-sequence payload stream :start pos))
       until (= pos (length payload)))
    (values (if (zerop decompressed-length)
                payload
                (let ((buffer (make-array decompressed-length :element-type '(unsigned-byte 8))))
                  (uiop/package:symbol-call
                   :chipz :decompress
                   buffer :zlib payload)
                  buffer))
            sequence-id)))

(defun write-compressed-wire-packet (stream payload &key (sequence-id 0))
  "Write PAYLOAD to STREAM as one or more compressed packets."
  (let* ((payload-length (length payload)))
    (flet ((send-payload (compressed-payload compressed-payload-length uncompressed-payload-length &key (start 0) end)
             (write-fixed-length-integer compressed-payload-length 3 stream)
             (write-byte sequence-id stream)
             (setf sequence-id (mod (1+ sequence-id) 256))
             (write-fixed-length-integer uncompressed-payload-length 3 stream)
             (write-sequence compressed-payload stream :start start :end end))
           (compress-payload (start end)
             (unless (typep payload '(simple-array (unsigned-byte 8) (*)))
               (setf payload (coerce payload '(simple-array (unsigned-byte 8) (*)))))
             (flexi-streams:with-output-to-sequence (buffer)
               (let ((compressor (make-instance (uiop/package:find-symbol* :zlib-compressor :salza2)
                                                :callback (lambda (octets end)
                                                            (write-sequence octets buffer :end end)))))
                 (uiop/package:symbol-call
                  :salza2 :compress-octet-vector
                  payload compressor :start start :end end)
                 (uiop/package:symbol-call
                  :salza2 :finish-compression
                  compressor)))))
      (if (< payload-length +mysql-minimum-compression-length+)
          (send-payload payload payload-length 0)
          (loop
            for length from payload-length downto 0 by #xffffff
            for start from 0 by #xffffff
            for max-end from #xffffff by #xffffff
            for end = (min (+ start length) max-end)
            for compressed-payload = (compress-payload start end)
            for compressed-payload-length = (length compressed-payload)
            do (if (< payload-length compressed-payload-length)
                   (send-payload payload payload-length 0 :start start :end end)
                   (send-payload compressed-payload compressed-payload-length payload-length))))))
  (force-output stream)
  sequence-id)

;;; Wrapper stream to implement the compressed protocol.
(defclass mysql-compressed-stream (trivial-gray-streams:trivial-gray-stream-mixin
                                   trivial-gray-streams:fundamental-binary-input-stream
                                   trivial-gray-streams:fundamental-binary-output-stream)
  ((stream :initarg :stream
           :accessor mysql-compressed-stream-stream
           :documentation "The underlying stream.")
   (input-buffer :type (or flexi-streams:in-memory-input-stream null)
                 :initform nil
                 :accessor mysql-compressed-stream-input-buffer
                 :documentation "The container for the incoming, just inflated, octet stream.")
   (output-buffer :type flexi-streams:in-memory-output-stream
                  :initform (flexi-streams:make-in-memory-output-stream)
                  :accessor mysql-compressed-stream-output-buffer
                  :documentation "The container for the outgoing, to be deflated, octet stream.")
   (sequence-id :type integer
                :initform 0
                :accessor mysql-compressed-stream-sequence-id
                :documentation "Sequence IDs for the compressed protocol packet stream.")))

(defun fill-input-buffer (stream &aux payload)
  "Allocates a new input buffer stream from the results of parsing a new
   compressed packet off of the wrapped stream. Requires that the existing
   input buffer, if any, be empty."
  (assert (typep stream 'mysql-compressed-stream))
  (with-accessors ((stream mysql-compressed-stream-stream)
                   (input-buffer mysql-compressed-stream-input-buffer)
                   (sequence-id mysql-compressed-stream-sequence-id))
      stream
    (assert (null (and input-buffer (listen input-buffer))))
    (multiple-value-setq (payload sequence-id)
      (read-compressed-wire-packet stream :expected-sequence-id sequence-id))
    (when input-buffer (close input-buffer))
    (setq input-buffer (flexi-streams:make-in-memory-input-stream payload))))

;;; Gray Stream methods for our compressed stream.
(defmethod trivial-gray-streams:stream-listen ((stream mysql-compressed-stream))
  (with-accessors ((stream mysql-compressed-stream-stream)
                   (input-buffer mysql-compressed-stream-input-buffer))
      stream
    (or (when input-buffer (listen input-buffer))
        (listen stream))))

(defmethod trivial-gray-streams:stream-read-byte ((stream mysql-compressed-stream))
  (with-accessors ((input-buffer mysql-compressed-stream-input-buffer))
      stream
    (unless (and input-buffer (listen input-buffer))
      (fill-input-buffer stream))
    (read-byte input-buffer)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream mysql-compressed-stream) sequence start end &key)
  (with-accessors ((input-buffer mysql-compressed-stream-input-buffer))
      stream
    (unless (and input-buffer (listen input-buffer))
      (fill-input-buffer stream))
    (read-sequence sequence input-buffer :start start :end end)))

(defmethod trivial-gray-streams:stream-write-byte ((stream mysql-compressed-stream) byte)
  (write-byte byte (mysql-compressed-stream-output-buffer stream)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream mysql-compressed-stream) sequence start end &key)
  (write-sequence sequence (mysql-compressed-stream-output-buffer stream) :start start :end end))

(defmethod trivial-gray-streams:stream-force-output ((stream mysql-compressed-stream))
  (with-accessors ((stream mysql-compressed-stream-stream)
                   (output-buffer mysql-compressed-stream-output-buffer)
                   (sequence-id mysql-compressed-stream-sequence-id))
      stream
    (setq sequence-id
          (write-compressed-wire-packet
           stream
           (flexi-streams:get-output-stream-sequence output-buffer)
           :sequence-id sequence-id))))
