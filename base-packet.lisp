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

(defun read-packet (stream &key (expected-sequence-id 0))
  (let ((payload (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
        (pos 0))
    (loop
      as length = (parse-fixed-length-integer stream 3)
      as sequence-id = (parse-fixed-length-integer stream 1)
      if (= sequence-id expected-sequence-id)
        do (setf expected-sequence-id (mod (1+ expected-sequence-id) 256))
      else
        do (error "Unexpected sequence id")
      end
      when (plusp length)
        do (adjust-array payload (+ pos length))
        and do (setf pos (read-sequence payload stream :start pos))
      when (< length #xffffff) return payload)))

(defun send-packet (stream payload &key (sequence-id 0))
  (loop
    for length from (length payload) by #x-ffffff
    for start from 0 by #xffffff
    for max-end from #xffffff by #xffffff
    for end = (min (+ start length) max-end)
    do (encode-fixed-length-integer stream (- end start) 3)
    do (write-byte sequence-id stream)
    do (setf sequence-id (mod (1+ sequence-id) 256))
    do (write-sequence payload stream :start start :end end)
    when (< length #xffffff) return (values)))
