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
• 3 octets        - length of packet
• 1 octet         - sequence id
• string[length] - the payload

5.1.2.1. Big packets
The largest packet is 2^24-1 octets long. If more data must be
transmitted, a series of packets of length #xffffff are sent, with
increasing sequence ids, until the remaining payload is less than
2^24-1 octets, at which point the remaining payload is transmitted with
its actual size. This means that a packet of exactly 2^24-1 octets is
transmitted as a full packet followed by a packet with a payload of 0
octets.

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
;;; payload octets.
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
;;;  - read-my-octet
;;;  - peek-my-octet
;;;  - read-my-sequence
;;;  - read-rest-of-packet-octets
;;;
;;; Higher level functions such as read-fixed-length-integer or
;;; read-length-encoded-integer are defined in basic-types.lisp and build on
;;; this low-level API.
;;;
(defconstant +max-packet-length+ #xffffff
  "Larger packet that we may receive, see
    https://dev.mysql.com/doc/internals/en/sending-more-than-16mbyte.html")

(defvar *max-allowed-packets* #xffffff
  "Client side implementation of max_allowed_packets.")

;;;
;;; Each packet len and pos is an (integer 0 #xffffff), but when a value
;;; expands multiple packet read-my-sequence will append to the same
;;; my-packet-stream instance the whole content in a single payload
;;; sequence, thus len might be more than #xffffff.
;;;
(defstruct (my-packet-stream (:conc-name my-))
  (source  nil :type (or null stream))
  (payload nil :type (or null (simple-array (unsigned-byte 8) *)))
  (seq-id  0   :type (integer 0 255))
  (len     0   :type integer)
  (pos     0   :type integer))

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
(declaim (inline %read-3-octets))

(defun %read-3-octets (stream)
  "Internal for wire protocol use only."
  (declare (type stream stream))
  ;; As we don't have a proper my-packet-stream yet, we can't use
  ;; the usual read-3-octets-integer implementation.
  ;; We also know we are reading unsigned integer...
  (let ((octet-1 (read-byte stream))
        (octet-2 (read-byte stream))
        (octet-3 (read-byte stream)))
    (declare (type (unsigned-byte 8) octet-1 octet-2 octet-3))
    (logior (ash octet-3 16) (ash octet-2  8) octet-1)))

(defun prepare-next-chunk (my-stream)
  "Prepare reading from a new packet from our source stream."
  (let ((expected-sequence-id (my-seq-id my-stream)))
    (setf (my-len my-stream)     (%read-3-octets (my-source my-stream))
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
  (declare (special *mysql-connection*))
  (flet ((read-next-chunk ()
           (prog1
               (prepare-next-chunk stream)
             ;; we have to care about the connection sequence id here
             (setf (mysql-connection-sequence-id *mysql-connection*)
                   (my-seq-id stream)))))
    (declare (inline read-next-chunk))
    (cond ((= (my-pos stream) +max-packet-length+)
           (read-next-chunk))

          ((= (my-pos stream) *max-allowed-packets*)
           (error "MySQL Packet too large: ~d is bigger than ~a bytes [~a]"
                  (my-len stream)
                  *max-allowed-packets*
                  (my-pos stream)))

          ((= (my-pos stream) (my-len stream))
           (if (< (my-len stream) +max-packet-length+)
               ;; no extra packet was needed
               (if eof-error-p (signal 'end-of-file) eof-value)

               ;; we reached the end of a +max-packet-length+ packet and need
               ;; to read from the next one now
               (read-next-chunk))))))

(defun read-my-octet (stream &optional eof-error-p eof-value)
  "Read a single octet from STREAM."
  (declare (type my-packet-stream stream))

  ;; support for the peek-my-octet API
  (maybe-read-next-chunk stream eof-error-p eof-value)

  ;; now read a single octet from our source
  (prog1
      (aref (my-payload stream) (my-pos stream))
    (incf (my-pos stream))))

(defun peek-first-octet (stream)
  "Get the first octet in this stream's chunk."
  (aref (my-payload stream) 0))

(defun read-my-sequence (sequence stream)
  "Copy data from the STREAM into SEQUENCE."
  (declare (type my-packet-stream stream)
           (type (simple-array (unsigned-byte 8) (*)) sequence))

  (if (<= (+ (length sequence) (my-pos stream)) (my-len stream))
      ;; we already have the octets we're asked for, just grab'em
      (progn
        (replace sequence (my-payload stream) :start2 (my-pos stream))
        (incf (my-pos stream) (length sequence)))

      ;; in that case we're going to cross at least a packet boundary
      (loop for pos fixnum from 0 below (length sequence)
         do (let* ((available-bytes (- (my-len stream) (my-pos stream)))
                   (bytes           (if (<= (+ pos available-bytes)
                                           (length sequence))
                                        available-bytes
                                        (- (length sequence) pos))))
              (replace sequence (my-payload stream)
                       :start1 pos
                       :start2 (my-pos stream)
                       :end2   (+ (my-pos stream) bytes))
              (incf (my-pos stream) bytes)
              (incf pos (- bytes 1))
              (maybe-read-next-chunk stream)))))

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
  "Read LENGTH octets from STREAM and return an array of them."
  (declare (type my-packet-stream stream))
  (let ((vector (make-array length :element-type '(unsigned-byte 8))))
    (loop for pos = 0 then (read-sequence vector (my-source stream) :start pos)
       until (= pos length))
    (incf (my-pos stream) length)
    vector))

(defun read-rest-of-packet-octets (stream)
  "Copy the rest of the whole data set into an array and return it."
  (declare (type my-packet-stream stream))

  (let* ((bytes-to-go (- (my-len stream) (my-pos stream)))
         (current-chunk-rest-bytes
          (make-array bytes-to-go :element-type '(unsigned-byte 8))))

    (when (< 0 bytes-to-go)
      (read-my-sequence current-chunk-rest-bytes stream))

    (loop for next-chunk = (maybe-read-next-chunk stream)
       while next-chunk

       collect (my-payload next-chunk) into vectors
       sum (my-len stream) into total-length

       finally
         (return
           (concatenate-vectors (+ bytes-to-go total-length)
                                (cons current-chunk-rest-bytes vectors))))))

;;;
;;; Write octets to the wire socket, as chunked packets.
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
