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
;;; asedeno-TODO: turn up compiler optimizations

;;; 15.1.1.1. Integer

;;; 15.1.1.1.1. fixed length integer
;;; Little endian fixed-length integers with lengths (1 2 3 4 6 8)

(defun read-fixed-length-integer (length stream)
  (let ((result 0))
    (loop
      repeat length
      for i fixnum from 0 by 8
      do (setf (ldb (byte 8 i) result) (read-byte stream)))
    result))

(defun write-fixed-length-integer (int length stream)
  (loop
    repeat length
    for i fixnum from 0 by 8
    do (write-byte (ldb (byte 8 i) int) stream)))

;;; 15.1.1.1.2. length encoded integer

(defun read-length-encoded-integer (stream)
  (let ((n (read-byte stream)))
    (cond
      ((< n #xfb) n)
      ;; #xfb here is undefined, though it may mean NULL in certain contexts.
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
