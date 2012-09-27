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

(defun parse-fixed-length-integer (stream length)
  (loop
     for i fixnum from 0 below length
     for n fixnum from 0 by 8
     as b fixnum = (read-byte stream)
     sum (ash b n)))

(defun encode-fixed-length-integer (stream int length)
  (loop
     for i fixnum from 0 below length
     for n fixnum from 0 by -8
     do (write-byte (logand (ash int n) #xff) stream)))

;;; 15.1.1.1.2. length encoded integer

(defun parse-length-encoded-integer (stream)
  (let ((n (read-byte stream)))
    (cond
      ((< n #xfb) n)
      ;; #xfb here is undefined, though it may mean NULL in certain contexts.
      ((= n #xfc)
       (parse-fixed-length-integer stream 2))
      ((= n #xfd)
       (parse-fixed-length-integer stream 3))
      ((= n #xfe)
       (parse-fixed-length-integer stream 8))
      ;; #xff here is undefined, though it may be an error packet in certain contexts.
      (t (error "bad length-encoded-integer")))))

(defun encode-length-encoded-integer (stream int)
  (cond
    ((< int 251)
     (write-byte int stream))
    ((< int #x10000)
     (write-byte #xfc stream)
     (encode-fixed-length-integer stream int 2))
    ((< int #x1000000)
     (write-byte #xfd stream)
     (encode-fixed-length-integer stream int 3))
    ((< int #x10000000000000000)
     (write-byte #xfe stream)
     (encode-fixed-length-integer stream int 8))
    (t (error "invalid input to encode-length-encoded-integer"))))
