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

;;; Utilities for reading/writing 3-byte integers to/from a stream for
;;; encoding packet length.
(defun read-ub24/le (stream)
  (loop
    for n fixnum from 0 by 8 below 24
    as b fixnum = (read-byte stream)
    sum (ash b n)))

(defun write-ub24/le (i stream)
  (assert (<= 0 i #xffffff))
  (write-byte (logand i #xff) stream)
  (write-byte (logand (ash i -8) #xff) stream)
  (write-byte (logand (ash i -16) #xff) stream)
  (values))

;;; Functions to read and write wire packets.
(defun read-wire-packet (stream &key (expected-sequence-id 0))
  (let ((payload (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
        (pos 0))
    (loop
      as length = (read-ub24/le stream)
      as sequence-id = (read-byte stream)
      if (= sequence-id expected-sequence-id)
        do (setf expected-sequence-id (mod (1+ expected-sequence-id) 256))
      else
        do (error "Unexpected sequence id")
      end
      when (plusp length)
        do (adjust-array payload (+ pos length))
        and do (setf pos (read-sequence payload stream :start pos))
      when (< length #xffffff) return (values payload expected-sequence-id))))

(defun write-wire-packet (stream payload &key (sequence-id 0))
  (loop
    for length from (length payload) downto 0 by #xffffff
    for start from 0 by #xffffff
    for max-end from #xffffff by #xffffff
    for end = (min (+ start length) max-end)
    do (write-ub24/le (- end start) stream)
    do (write-byte sequence-id stream)
    do (setf sequence-id (mod (1+ sequence-id) 256))
    do (write-sequence payload stream :start start :end end))
  (force-output stream)
  sequence-id)
