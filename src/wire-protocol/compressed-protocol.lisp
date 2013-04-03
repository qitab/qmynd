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

;;; Functions to read and write wire packets for the compressed protocol.
(defun read-compressed-wire-packet (stream &key (expected-sequence-id 0))
  (let (payload
        (pos 0)
        (compressed-length (read-fixed-length-integer 3 stream))
        (sequence-id (if (= (read-byte stream) expected-sequence-id)
                         (setf expected-sequence-id (mod (1+ expected-sequence-id) 256))
                         (error "Unexpected sequence id")))
        (decompressed-length (read-fixed-length-integer 3 stream)))
    (assert (plusp compressed-length))
    (setf payload (make-array compressed-length :element-type '(unsigned-byte 8)))
    (loop do (setf pos (read-sequence payload stream :start pos))
          until (= pos (length payload)))
    (values (if (zerop decompressed-length)
                payload
                (let ((buffer (make-array decompressed-length :element-type '(unsigned-byte 8))))
                  (uiop/package:symbol-call
                   :chipz :decompress
                   buffer (uiop/package:find-symbol* :zlib :chipz) payload)
                  buffer))
            sequence-id)))

(defun write-compressed-wire-packet (stream payload &key (sequence-id 0))
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
           :documentation "The underlying stream")
   (input-buffer :type (or flexi-streams:in-memory-input-stream null)
                 :initform nil
                 :accessor mysql-compressed-stream-input-buffer
                 :documentation "The container for the incoming, just inflated, byte stream.")
   (output-buffer :type flexi-streams:in-memory-output-stream
                  :initform (flexi-streams:make-in-memory-output-stream)
                  :accessor mysql-compressed-stream-output-buffer
                  :documentation "The container for the outgoing, to be deflated, byte stream.")
   (sequence-id :type integer
                :initform 0
                :accessor mysql-compressed-stream-sequence-id)))

(defun fill-input-buffer (stream)
  "Allocates a new input buffer stream from the results of parsing a new compressed packet off of
   the captured stream"
  (assert (typep stream 'mysql-compressed-stream))
  (let ((input-buffer (mysql-compressed-stream-input-buffer stream)))
    (assert (null (and input-buffer (listen input-buffer)))))
  (multiple-value-bind (payload sequence-id)
      (read-compressed-wire-packet (mysql-compressed-stream-stream stream)
                                   :expected-sequence-id (mysql-compressed-stream-sequence-id stream))
    (setf (mysql-compressed-stream-input-buffer stream)
          (flexi-streams:make-in-memory-input-stream payload))
    (setf (mysql-compressed-stream-sequence-id stream) sequence-id)))

;;; Gray Stream methods for our compressed stream.
(defmethod trivial-gray-streams:stream-listen ((stream mysql-compressed-stream))
  (or (let ((input-buffer (mysql-compressed-stream-input-buffer stream)))
        (when input-buffer
          (listen input-buffer)))
      (listen (mysql-compressed-stream-stream stream))))

(defmethod trivial-gray-streams:stream-read-byte ((stream mysql-compressed-stream))
  (let ((input-buffer (mysql-compressed-stream-input-buffer stream)))
    (unless (and input-buffer (listen input-buffer))
      (fill-input-buffer stream)))
  (read-byte (mysql-compressed-stream-input-buffer stream)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream mysql-compressed-stream) sequence start end &key)
  (let ((input-buffer (mysql-compressed-stream-input-buffer stream)))
    (unless (and input-buffer (listen input-buffer))
      (fill-input-buffer stream)))
  (read-sequence sequence (mysql-compressed-stream-input-buffer stream) :start start :end end))

(defmethod trivial-gray-streams:stream-write-byte ((stream mysql-compressed-stream) byte)
  (write-byte byte (mysql-compressed-stream-output-buffer stream)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream mysql-compressed-stream) sequence start end &key)
  (write-sequence sequence (mysql-compressed-stream-output-buffer stream) :start start :end end))

(defmethod trivial-gray-streams:stream-force-output ((stream mysql-compressed-stream))
  (setf (mysql-compressed-stream-sequence-id stream)
        (write-compressed-wire-packet (mysql-compressed-stream-stream stream)
                                      (flexi-streams:get-output-stream-sequence
                                       (mysql-compressed-stream-output-buffer stream))
                                      :sequence-id (mysql-compressed-stream-sequence-id stream))))
