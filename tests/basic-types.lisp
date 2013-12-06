;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-test)

(defmacro with-packet-input ((stream sequence) &body body)
  `(call-with-packet-input ,sequence (lambda (,stream) ,@body)))

(defun call-with-packet-input (sequence fun)
  (funcall fun
           (qmynd-impl::make-my-packet-stream
            :source (flexi-streams:make-in-memory-input-stream ())
            :payload (coerce sequence '(vector (unsigned-byte 8)))
            :seq-id 1 :pos 0 :len (length sequence))))

(define-test decode-fixed-length-integers ()
  ;;prepare a stream with a bunch of integers for decoding
  (with-packet-input (s #(#x00  #x10  #x80  #xff
                          #x00 #x00  #xfe #xff
                          #x00 #x00 #x00  #xfd #xfe #xff
                          #x00 #x00 #x00 #x00  #xfc #xfd #xfe #xff
                          #x00 #x00 #x00 #x00 #x00 #x0
                          #xfa #xfb #xfc #xfd #xfe #xff
                          #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x0
                          #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff
                          #xff  #x7f  #x80  #x00
                          #xff #xff  #xff #x7f
                          #x00 #x80  #x00 #x00))
    ;; 1 octet
    (assert-equal
     (read-fixed-length-integer 1 s)
     #x0)
    (assert-equal
     (read-fixed-length-integer 1 s)
     #x10)
    (assert-equal
     (read-fixed-length-integer 1 s)
     #x80)
    (assert-equal
     (read-fixed-length-integer 1 s)
     #xff)
    ;; 2 octet
    (assert-equal
     (read-fixed-length-integer 2 s)
     #x0)
    (assert-equal
     (read-fixed-length-integer 2 s)
     #xfffe)
    ;; 3 octet
    (assert-equal
     (read-fixed-length-integer 3 s)
     #x0)
    (assert-equal
     (read-fixed-length-integer 3 s)
     #xfffefd)
    ;; 4 octet
    (assert-equal
     (read-fixed-length-integer 4 s)
     #x0)
    (assert-equal
     (read-fixed-length-integer 4 s)
     #xfffefdfc)
    ;; 6 octet
    (assert-equal
     (read-fixed-length-integer 6 s)
     #x0)
    (assert-equal
     (read-fixed-length-integer 6 s)
     #xfffefdfcfbfa)
    ;; 8 octet
    (assert-equal
     (read-fixed-length-integer 8 s)
     #x0)
    (assert-equal
     (read-fixed-length-integer 8 s)
     #xfffefdfcfbfaf9f8)
    ;; Signed integers
    (assert-equal
     (read-fixed-length-integer 1 s :signed t)
     -1)
    (assert-equal
     (read-fixed-length-integer 1 s :signed t)
     127)
    (assert-equal
     (read-fixed-length-integer 1 s :signed t)
     -128)
    (assert-equal
     (read-fixed-length-integer 1 s :signed t)
     0)
    (assert-equal
     (read-fixed-length-integer 2 s :signed t)
     -1)
    (assert-equal
     (read-fixed-length-integer 2 s :signed t)
     32767)
    (assert-equal
     (read-fixed-length-integer 2 s :signed t)
     -32768)
    (assert-equal
     (read-fixed-length-integer 2 s :signed t)
     0)))

(define-test encode-fixed-length-integers ()
  (flet ((encode-test (int len expected)
           (assert-equal
            (flexi-streams:with-output-to-sequence (s)
              (write-fixed-length-integer int len s))
            expected :test #'equalp)))
    ;; 1 octet
    (encode-test 0 1 #(0))
    (encode-test #x10 1 #(#x10))
    (encode-test #x80 1 #(#x80))
    (encode-test #xff 1 #(#xff))
    (encode-test -1 1 #(#xff))
    (encode-test 127 1 #(#x7f))
    ;; 1 octet fun with aliasing
    (encode-test 128 1 #(#x80))
    (encode-test -128 1 #(#x80))
    ;; 2 octet
    (encode-test 0 2 #(0 0))
    (encode-test #xfffe 2 #(#xfe #xff))
    (encode-test -1 2 #(#xff #xff))
    ;; 3 octet
    (encode-test 0 3 #(0 0 0))
    (encode-test #xfffefd 3 #(#xfd #xfe #xff))
    ;; 4 octet
    (encode-test 0 4 #(0 0 0 0))
    (encode-test #xfffefdfc 4 #(#xfc #xfd #xfe #xff))
    ;; 6 octet
    (encode-test 0 6 #(0 0 0 0 0 0))
    (encode-test #xfffefdfcfbfa 6 #(#xfa #xfb #xfc #xfd #xfe #xff))
    ;; 8 octet
    (encode-test 0 8 #(0 0 0 0 0 0 0 0))
    (encode-test #xfffefdfcfbfaf9f8 8 #(#xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff))))

(define-test decode-length-encoded-integers ()
  (with-packet-input (s #(#x0
                          #x80
                          #xfa
                          #xfc #xfb #x0
                          #xfc #xfc #x0
                          #xfc #xfe #xff
                          #xfd #xfd #xfe #xff
                          #xfe #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff
                          ))
    (assert-equal
     (read-length-encoded-integer s)
     #x0)
    (assert-equal
     (read-length-encoded-integer s)
     #x80)
    (assert-equal
     (read-length-encoded-integer s)
     #xfa)
    (assert-equal
     (read-length-encoded-integer s)
     #xfb)
    (assert-equal
     (read-length-encoded-integer s)
     #xfc)
    (assert-equal
     (read-length-encoded-integer s)
     #xfffe)
    (assert-equal
     (read-length-encoded-integer s)
     #xfffefd)
    (assert-equal
     (read-length-encoded-integer s)
     #xfffefdfcfbfaf9f8)))

(define-test encode-length-encoded-integers ()
  (flet ((encode-test (int expected)
           (assert-equal
            (flexi-streams:with-output-to-sequence (s)
              (write-length-encoded-integer int s))
            expected :test #'equalp)))
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
    (with-packet-input (s (concatenate '(vector (unsigned-byte 8))
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
                                       #(0)))
      ;; Pull strings out of the stream.
      (assert-equal
       (babel:octets-to-string (read-fixed-length-octets 7 s))
       "Testing"
       :test #'string=)

      (assert-equal
       (babel:octets-to-string (read-length-encoded-octets s))
       "Hello, world!"
       :test #'string=)

      (assert-equal
       (babel:octets-to-string (read-null-terminated-octets s))
       "Hello"
       :test #'string=)

      (let ((str (babel:octets-to-string (read-length-encoded-octets s))))
        (assert-equal (length str) 251)
        (assert-true (every #'(lambda (x) (char= x #\A)) str)))

      (let ((str (babel:octets-to-string (read-null-terminated-octets s))))
        (assert-equal (length str) 256)
        (assert-true (every #'(lambda (x) (char= x #\A)) str)))

      (assert-equal
       (babel:octets-to-string (read-null-terminated-octets s))
       "Goodbye"
       :test #'string=))))

(define-test-suite mysql-basic-types-suite ()
  (decode-fixed-length-integers
   encode-fixed-length-integers
   decode-length-encoded-integers
   encode-length-encoded-integers
   decode-strings))

(register-test 'mysql-basic-types-suite)
