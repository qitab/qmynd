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

;;; 15.1.2. MySQL Packet

#|
A MySQL Packet consists of:
• 3 bytes        - length of packet
• 1 byte         - sequence id
• string[length] - the payload

5.1.2.1. Big packets
The largest packet is 2^24-1 bytes long. If more data must be
transmitted, a series of packets of length #xffffff are sent, with
increasing sequence ids, until the remaining payload is less than
2^24-1 bytes, at which point the remaining payload is transmitted with
its actual size. This means that a packet of exactly 2^24-1 bytes is
transmitted as a full packet followed by a packet with a payload of 0
bytes.

5.1.2.2. Sequence IDs
The sequence_id is allowed to wrap. It is reset to 0 at the start of
each Command Phase.

|#

;;;
;;; The my-packet-stream class actually represents a chunk of the payload at
;;; a time, and its methods below know how to read from a packet and skip to
;;; the next chunk as needed.
;;;
;;; Each time we switch from a chunk to the next while reading the current
;;; packet, the slots of the my-stream instance are reset to whatever the
;;; next chunk properties are, including the chunk contents: we preload the
;;; payload bytes.
;;;
;;; The methods are implemented as functions because we care enough about
;;; performances here to want to avoid the cost of CLOS dispatching.
;;;
;;; The main entry point to read data from the raw stream is
;;; read-wire-packet, which prepares a packet by reading its first chunk.
;;;
;;; The define-packet API and basic types implementation then fetch data
;;; from the stream by using the following low-level functions:
;;;
;;;  - read-my-byte
;;;  - peek-my-byte
;;;  - read-my-sequence
;;;  - read-all-remaining-bytes
;;;
;;; Higher level functions such as read-fixed-length-integer or
;;; read-length-encoded-integer are defined in basic-types.lisp and build on
;;; this low-level API.
;;;
(defconstant +max-packet-length+ #xffffff)

(defstruct (my-packet-stream (:conc-name my-))
  (source  nil :type (or null stream))
  (payload nil :type (or null (simple-array (unsigned-byte 8) *)))
  (seq-id  0   :type (integer 0 255))
  (len     0   :type (integer 0 #xffffff))
  (pos     0   :type (integer 0 #xffffff)))

(defmethod print-object ((stream my-packet-stream) out)
  (print-unreadable-object (stream out :type t)
    (format out "~d/~d [~d]"
            (if (slot-boundp stream 'pos) (my-pos stream) "-")
            (if (slot-boundp stream 'len) (my-len stream) "-")
            (my-seq-id stream))))

(defun read-wire-packet (stream &key (expected-sequence-id 0))
  "Instanciate a my-packet-stream object and read some meta-data about it."
  (let ((my-stream (make-my-packet-stream :source stream
                                          :seq-id expected-sequence-id)))
    ;; the next chunk is going to be the first one
    (prepare-next-chunk my-stream)))

;;;
;;; Low level protocol handling
;;;
(declaim (inline %read-3-bytes))

(defun %read-3-bytes (stream)
  "Internal for wire protocol use only."
  (declare (type stream stream))
  ;; As we don't have a proper my-packet-stream yet, we can't use
  ;; the usual read-3-bytes-integer implementation. We also know we
  ;; are readding unsigned integer...
  (let ((byte-1 (read-byte stream))
        (byte-2 (read-byte stream))
        (byte-3 (read-byte stream)))
    (declare (type (unsigned-byte 8) byte-1 byte-2 byte-3))
    (logior (ash byte-3 16) (ash byte-2  8) byte-1)))

(defun prepare-next-chunk (my-stream)
  "Prepare reading from a new packet from our source stream."
  (let ((expected-sequence-id (my-seq-id my-stream)))
    (setf (my-len my-stream)     (%read-3-bytes (my-source my-stream))
          (my-seq-id my-stream)  (read-byte (my-source my-stream)))

    (assert (= expected-sequence-id (my-seq-id my-stream))
            () 'unexpected-sequence-id)

    ;; prefetch this chunk of data, and reset the position
    (setf (my-payload my-stream)
          (read-whole-chunk (my-len my-stream) my-stream)

          (my-pos my-stream) 0)

    (setf (my-seq-id my-stream) (logand (1+ expected-sequence-id) #xFF))
    (values my-stream (my-seq-id my-stream))))

;;;
;;; Streaming API on top of the chunked packets protocol, automatically
;;; switch to next chunk as we read from the current packet.
;;;
(declaim (inline maybe-read-next-chunk))
(defun maybe-read-next-chunk (stream &optional eof-error-p eof-value)
  "Check if we are at the end of the packet, or if we need to read from the
   next chunk from the network within the same \"logical\" packet."
  (when (= (my-pos stream) (my-len stream))
    (if (< (my-len stream) +max-packet-length+)
      ;; no extra packet was needed
      (if eof-error-p (signal 'end-of-file) eof-value)

      ;; we reached the end of a +max-packet-length+ packet and need to read
      ;; from the next one now
      (progn
        (prepare-next-chunk stream)
        ;; we have to care about the connection sequence id here
        (setf (mysql-connection-sequence-id *mysql-connection*)
              (my-seq-id stream))))))

(defun read-my-byte (stream &optional eof-error-p eof-value)
  "Read a single byte from STREAM."
  (declare (type my-packet-stream stream))

  ;; support for the peek-my-byte API
  (maybe-read-next-chunk stream eof-error-p eof-value)

  ;; now read a single byte from our source
  (prog1
      (aref (my-payload stream) (my-pos stream))
    (incf (my-pos stream))))

(defun peek-first-byte (stream)
  "Get the first byte in this stream's chunk."
  (aref (my-payload stream) 0))

(defun read-my-sequence (sequence stream)
  "Copy data from the STREAM into SEQUENCE."
  (declare (type my-packet-stream stream)
           (type (simple-array (unsigned-byte 8) (*)) sequence))

  (if (<= (+ (length sequence) (my-pos stream)) (my-len stream))
      ;; we already have the bytes we're asked for, just grab'em
      (progn
        (replace sequence (my-payload stream) :start2 (my-pos stream))
        (incf (my-pos stream) (length sequence)))

      ;; in that case we're going to cross a packet boundary, so read one
      ;; byte at a time, read-my-byte knows how to cross boundaries.
      (loop
         for pos fixnum from 0 below (length sequence)
         do (setf (aref sequence pos) (read-my-byte stream))
         finally (return pos))))

;;;
;;; API to finish reading all remaining chunks of a packet
;;;
(defun concatenate-vectors (length vectors)
  "Given a list of VECTORS containing LENGTH octets in total, return a
   single vector containing the same octets in the same order."
  (if (= 1 (length vectors))
      (first vectors)
      (let ((vector (make-array length :element-type '(unsigned-byte 8))))
        (loop for start = 0 then (+ start (length sub-vector))
           for sub-vector in vectors
           do (replace vector
                       (the (simple-array (unsigned-byte 8) (*))
                            sub-vector)
                       :start1 start))
        vector)))

(defun read-whole-chunk (length stream)
  "Read LENGTH bytes from STREAM and return an array of them."
  (declare (type my-packet-stream stream))
  (let ((vector (make-array length :element-type '(unsigned-byte 8))))
    (loop for pos = 0 then (read-sequence vector (my-source stream) :start pos)
       until (= pos length))
    (incf (my-pos stream) length)
    vector))

(defun read-all-remaining-bytes (stream)
  "Copy the rest of the whole data set into an array and return it."
  (declare (type my-packet-stream stream))

  (loop
     for length = (- (my-len stream) (my-pos stream))

     when (plusp length)
     collect (read-whole-chunk length stream) into vectors
     and sum length into total-length

     ;; we might read less than the full length of the current chunk, but we
     ;; only get to stop when the current chunk total length is less than
     ;; +max-packet-length+
     if (< (my-len stream) +max-packet-length+)
     return (concatenate-vectors total-length vectors)
     else do (prepare-next-chunk stream)))

;;;
;;; Write bytes to the wire socket, as chunked packets.
;;;
(defun write-wire-packet (stream payload &key (sequence-id 0))
  "Write PAYLOAD to STREAM as one or more packets."
  (loop
    for length from (length payload) downto 0 by #xffffff
    for start from 0 by #xffffff
    for max-end from #xffffff by #xffffff
    for end = (min (+ start length) max-end)
    do (write-fixed-length-integer (- end start) 3 stream)
    do (write-byte sequence-id stream)
    do (setf sequence-id (mod (1+ sequence-id) 256))
    do (write-sequence payload stream :start start :end end))
  (force-output stream)
  sequence-id)
