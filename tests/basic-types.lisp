;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mysqlnd-test)

(define-test decode-fixed-length-integers ()
  ;;prepare a stream with a bunch of integers for decoding
  (with-open-stream (s (flexi-streams:make-in-memory-input-stream #(#x00 #x10 #x80 #xff
                                                                    #x00 #x00 #xfe #xff
                                                                    #x00 #x00 #x0
                                                                    #xfd #xfe #xff
                                                                    #x00 #x00 #x00 #x0
                                                                    #xfc #xfd #xfe #xff
                                                                    #x00 #x00 #x00 #x00 #x00 #x0
                                                                    #xfa #xfb #xfc #xfd #xfe #xff
                                                                    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x0
                                                                    #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff)))

    ;; 1 byte
    (assert-equal
     (parse-fixed-length-integer s 1)
     #x0)
    (assert-equal
     (parse-fixed-length-integer s 1)
     #x10)
    (assert-equal
     (parse-fixed-length-integer s 1)
     #x80)
    (assert-equal
     (parse-fixed-length-integer s 1)
     #xff)
    ;; 2 byte
    (assert-equal
     (parse-fixed-length-integer s 2)
     #x0)
    (assert-equal
     (parse-fixed-length-integer s 2)
     #xfffe)
    ;; 3 byte
    (assert-equal
     (parse-fixed-length-integer s 3)
     #x0)
    (assert-equal
     (parse-fixed-length-integer s 3)
     #xfffefd)
    ;; 4 byte
    (assert-equal
     (parse-fixed-length-integer s 4)
     #x0)
    (assert-equal
     (parse-fixed-length-integer s 4)
     #xfffefdfc)
    ;; 6 byte
    (assert-equal
     (parse-fixed-length-integer s 6)
     #x0)
    (assert-equal
     (parse-fixed-length-integer s 6)
     #xfffefdfcfbfa)
    ;; 8 byte
    (assert-equal
     (parse-fixed-length-integer s 8)
     #x0)
    (assert-equal
     (parse-fixed-length-integer s 8)
     #xfffefdfcfbfaf9f8)))

(define-test encode-fixed-length-integers ()
  (flet ((encode-test (int len expected)
           (let ((stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
             (encode-fixed-length-integer stream int len)
             (assert-equal (flexi-streams:get-output-stream-sequence stream)
                           expected :test equalp))))
    ;; 1 byte
    (encode-test 0 1 #(0))
    (encode-test #x10 1 #(#x10))
    (encode-test #x80 1 #(#x80))
    (encode-test #xff 1 #(#xff))
    ;; 2 byte
    (encode-test 0 2 #(0 0))
    (encode-test #xfffe 2 #(#xfe #xff))
    ;; 3 byte
    (encode-test 0 3 #(0 0 0))
    (encode-test #xfffefd 3 #(#xfd #xfe #xff))
    ;; 4 byte
    (encode-test 0 4 #(0 0 0 0))
    (encode-test #xfffefdfc 4 #(#xfc #xfd #xfe #xff))
    ;; 6 byte
    (encode-test 0 6 #(0 0 0 0 0 0))
    (encode-test #xfffefdfcfbfa 6 #(#xfa #xfb #xfc #xfd #xfe #xff))
    ;; 8 byte
    (encode-test 0 8 #(0 0 0 0 0 0 0 0))
    (encode-test #xfffefdfcfbfaf9f8 8 #(#xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff))))

(define-test decode-length-encoded-integers ()
  (with-open-stream (s (flexi-streams:make-in-memory-input-stream #(#x0
                                                                    #x80
                                                                    #xfa
                                                                    #xfc #xfb #x0
                                                                    #xfc #xfc #x0
                                                                    #xfc #xfe #xff
                                                                    #xfd #xfd #xfe #xff
                                                                    #xfe #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff
                                                                    )))
    (assert-equal
     (parse-length-encoded-integer s)
     #x0)
    (assert-equal
     (parse-length-encoded-integer s)
     #x80)
    (assert-equal
     (parse-length-encoded-integer s)
     #xfa)
    (assert-equal
     (parse-length-encoded-integer s)
     #xfb)
    (assert-equal
     (parse-length-encoded-integer s)
     #xfc)
    (assert-equal
     (parse-length-encoded-integer s)
     #xfffe)
    (assert-equal
     (parse-length-encoded-integer s)
     #xfffefd)
    (assert-equal
     (parse-length-encoded-integer s)
     #xfffefdfcfbfaf9f8)))

(define-test encode-length-encoded-integers ()
  (flet ((encode-test (int expected)
           (let ((stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
             (encode-length-encoded-integer stream int)
             (assert-equal (flexi-streams:get-output-stream-sequence stream)
                           expected :test equalp))))
    (encode-test #x00 #(#x00))
    (encode-test #x80 #(#x80))
    (encode-test #xfa #(#xfa))
    (encode-test #xfb #(#xfc #xfb #x00))
    (encode-test #xfc #(#xfc #xfc #x00))
    (encode-test #xfffe #(#xfc #xfe #xff))
    (encode-test #xfffefd #(#xfd #xfd #xfe #xff))
    (encode-test #xfffefdfcfbfaf9f8 #(#xfe #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff))))

(define-test decode-strings ()
  (let ((babel:*default-character-encoding* :utf-8))
    ;; Preparing an octet stream with a bunch of strings in it.
    (with-open-stream (s (flexi-streams:make-in-memory-input-stream (concatenate '(vector (unsigned-byte 8))
                                                                                 (babel:string-to-octets "Testing")
                                                                                 #(13)
                                                                                 (babel:string-to-octets "Hello, world!")
                                                                                 (babel:string-to-octets "Hello")
                                                                                 #(0)
                                                                                 #(#xfc #xfb #x0)
                                                                                 (make-array #xfb
                                                                                             :element-type '(unsigned-byte 8)
                                                                                             :initial-element #x41)
                                                                                 (make-array #x100
                                                                                             :element-type '(unsigned-byte 8)
                                                                                             :initial-element #x41)
                                                                                 #(0)
                                                                                 (babel:string-to-octets "Goodbye")
                                                                                 #(0))))

      ;; Pull strings out of the stream.
      (assert-equal
       (parse-fixed-length-string s 7)
       "Testing"
       :test string=)

      (assert-equal
      (parse-length-encoded-string s)
       "Hello, world!"
       :test string=)

      (assert-equal
       (parse-null-terminated-string s)
       "Hello"
       :test string=)

      (let ((str (parse-length-encoded-string s)))
        (assert-equal (length str) 251)
        (assert-true (every #'(lambda (x) (char= x #\A)) str)))

      (let ((str (parse-null-terminated-string s)))
        (assert-equal (length str) 256)
        (assert-true (every #'(lambda (x) (char= x #\A)) str)))

      (assert-equal
       (parse-null-terminated-string s)
       "Goodbye"
       :test string=))))

(define-test-suite mysql-basic-types-suite ()
  (decode-fixed-length-integers)
  (encode-fixed-length-integers)
  (decode-length-encoded-integers)
  (encode-length-encoded-integers)
  (decode-strings))

(register-test 'mysql-basic-types-suite)
