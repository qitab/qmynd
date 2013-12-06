;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB: The unicode tests will fail if this file is not parsed as UTF-8.

(in-package :qmynd-test)

(defun sequence-equal (x y)
  ;; smooths over difference in type, fill-pointer, etc.
  (and (typep x 'sequence) (typep y 'sequence)
       (equal (coerce x 'list) (coerce y 'list))))

(defmacro generate-binary-encoding-test (value expected-value-encoding expected-type-encoding)
  `(generate-binary-encoding-test-helper
    ',value ,value ,expected-value-encoding ,expected-type-encoding))

(defun generate-binary-encoding-test-helper
    (value-form value expected-value-encoding expected-type-encoding)
  (let ((vs (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
        (ts (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (encode-binary-parameter value vs ts)
           (assert-equal-helper
            `(binary-encoding ,value-form :value) (flexi-streams:get-output-stream-sequence vs)
            'expected expected-value-encoding 'sequence-equal)
           (assert-equal-helper
            `(binary-encoding ,value-form :type) (flexi-streams:get-output-stream-sequence ts)
            'expected expected-type-encoding 'sequence-equal))
         (when vs (close vs))
         (when ts (close ts)))))

;; Octets
(define-test binary-encoding-octets-1 ()
  (let ((octets (coerce #(0 1 2 3 127 128 255) '(vector (unsigned-byte 8)))))
    (generate-binary-encoding-test
     octets
     (concatenate '(vector (unsigned-byte 8)) #(7) octets)
     #(#.+mysql-type-var-string+ #x00))))
(define-test binary-encoding-octets-2 ()
  (let ((octets (make-array 255 :element-type '(unsigned-byte 8) :initial-element 0)))
    (generate-binary-encoding-test
     octets
     (concatenate '(vector (unsigned-byte 8)) #(#xfc #xff #x00) octets)
     #(#.+mysql-type-var-string+ #x00))))

(define-test-suite binary-encoding-octets-suite ()
  (binary-encoding-octets-1
   binary-encoding-octets-2))

;; Strings
(define-test binary-encoding-ascii-string ()
  (generate-binary-encoding-test
   "Hello, World!"
   #(13 72 101 108 108 111 44 32 87 111 114 108 100 33)
   #(#.+mysql-type-var-string+ #x00)))
(define-test binary-encoding-unicode-string ()
  (generate-binary-encoding-test
   "拝啓" ;; U+62DD U+5553, just for some unicode testing
   #(6 230 139 157 229 149 147)
   #(#.+mysql-type-var-string+ #x00)))
(define-test binary-encoding-unicode-string-2 ()
  (generate-binary-encoding-test
   "Sedeño"
   #(7 83 101 100 101 195 177 111)
   #(#.+mysql-type-var-string+ #x00)))
(define-test binary-encoding-latin-1-string ()
  (let ((babel::*default-character-encoding* :latin-1))
    (generate-binary-encoding-test
     "Sedeño"
     #(6 83 101 100 101 241 111)
     #(#.+mysql-type-var-string+ #x00))))

(define-test-suite binary-encoding-string-suite ()
  (binary-encoding-ascii-string
   binary-encoding-unicode-string
   binary-encoding-unicode-string-2
   binary-encoding-latin-1-string))

;; Integers (signed if negative)
(define-test binary-encoding-tiny-1 ()
  (generate-binary-encoding-test #x00  #(#x00) #(#.+mysql-type-tiny+ #x80)))
(define-test binary-encoding-tiny-2 ()
  (generate-binary-encoding-test #x80  #(#x80) #(#.+mysql-type-tiny+ #x80)))
(define-test binary-encoding-tiny-3 ()
  (generate-binary-encoding-test #xff  #(#xff) #(#.+mysql-type-tiny+ #x80)))
(define-test binary-encoding-tiny-4 ()
  (generate-binary-encoding-test #x-80 #(#x80) #(#.+mysql-type-tiny+ #x00)))
(define-test binary-encoding-tiny-5 ()
  (generate-binary-encoding-test #x-01 #(#xff) #(#.+mysql-type-tiny+ #x00)))
;; short
(define-test binary-encoding-short-1 ()
  (generate-binary-encoding-test #x8000  #(#x00 #x80) #(#.+mysql-type-short+ #x80)))
(define-test binary-encoding-short-2 ()
  (generate-binary-encoding-test #xffff  #(#xff #xff) #(#.+mysql-type-short+ #x80)))
(define-test binary-encoding-short-3 ()
  (generate-binary-encoding-test #x-8000 #(#x00 #x80) #(#.+mysql-type-short+ #x00)))
;; long
(define-test binary-encoding-long-1 ()
  (generate-binary-encoding-test #x80000000  #(#x00 #x00 #x00 #x80) #(#.+mysql-type-long+ #x80)))
(define-test binary-encoding-long-2 ()
  (generate-binary-encoding-test #xffffffff  #(#xff #xff #xff #xff) #(#.+mysql-type-long+ #x80)))
(define-test binary-encoding-long-3 ()
  (generate-binary-encoding-test #x-80000000 #(#x00 #x00 #x00 #x80) #(#.+mysql-type-long+ #x00)))
;; long long
(define-test binary-encoding-longlong-1 ()
  (generate-binary-encoding-test #x8000000000000000  #(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80) #(#.+mysql-type-longlong+ #x80)))
(define-test binary-encoding-longlong-2 ()
  (generate-binary-encoding-test #xffffffffffffffff  #(#xff #xff #xff #xff #xff #xff #xff #xff) #(#.+mysql-type-longlong+ #x80)))
(define-test binary-encoding-longlong-3 ()
  (generate-binary-encoding-test #x-8000000000000000 #(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80) #(#.+mysql-type-longlong+ #x00)))

(define-test-suite binary-encoding-integer-suite ()
  (binary-encoding-tiny-1
   binary-encoding-tiny-2
   binary-encoding-tiny-3
   binary-encoding-tiny-4
   binary-encoding-tiny-5
   binary-encoding-short-1
   binary-encoding-short-2
   binary-encoding-short-3
   binary-encoding-long-1
   binary-encoding-long-2
   binary-encoding-long-3
   binary-encoding-longlong-1
   binary-encoding-longlong-2
   binary-encoding-longlong-3))

;; Ratios
;; as decimals, which travel as length-encoded strings.
(define-test binary-encoding-ratio-decimal-1 ()
  (generate-binary-encoding-test
   (/ 102 10)
   #(4 #x31 #x30 #x2e #x32)
   #(#.+mysql-type-var-string+ #x00)))
(define-test binary-encoding-ratio-decimal-2 ()
  (generate-binary-encoding-test
   (/ -102 10)
   #(5 #x2d #x31 #x30 #x2e #x32)
   #(#.+mysql-type-var-string+ #x00)))
;; approximated as a double when not a decimal
(define-test binary-encoding-ratio-double-1 ()
  (generate-binary-encoding-test
   (/ 102 11)
   #(#x8C #x2E #xBA #xE8 #xA2 #x8B #x22 #x40)
   #(#.+mysql-type-double+ #x80)))
(define-test binary-encoding-ratio-double-2 ()
  (generate-binary-encoding-test
   (/ -102 11)
   #(#x8C #x2E #xBA #xE8 #xA2 #x8B #x22 #xC0)
   #(#.+mysql-type-double+ #x80)))

(define-test-suite binary-encoding-ratio-suite ()
  (binary-encoding-ratio-decimal-1
   binary-encoding-ratio-decimal-2
   binary-encoding-ratio-double-1
   binary-encoding-ratio-double-2))

;; Floating Point
;; single - expects lisp single-float to be encoded as 32-bit
(define-test binary-encoding-float-1 ()
  (generate-binary-encoding-test
   10.2f0
   #(#x33 #x33 #x23 #x41)
   #(#.+mysql-type-float+ #x80)))
;; double - expects lisp double-float to be encoded as 64-bit
(define-test binary-encoding-double-1 ()
  (generate-binary-encoding-test
   10.2d0
   #(#x66 #x66 #x66 #x66 #x66 #x66 #x24 #x40)
   #(#.+mysql-type-double+ #x80)))

(define-test-suite binary-encoding-float-suite ()
  (binary-encoding-float-1
   binary-encoding-double-1))

;; MySQL Date/Time Structs
;; Date-Time, at lengths 0, 4, 7, and 11
(define-test binary-encoding-datetime-1 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-date-time)
   #(#x00)
   #(#.+mysql-type-datetime+ #x00)))
(define-test binary-encoding-datetime-2 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-date-time :year 1900 :month 1 :day 2)
   #(#x04 #x6c #x07 #x01 #x02)
   #(#.+mysql-type-datetime+ #x00)))
(define-test binary-encoding-datetime-3 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-date-time :year 1900 :month 1 :day 2 :hour 3 :minute 4 :second 5 )
   #(#x07 #x6c #x07 #x01 #x02 #x03 #x04 #x05)
   #(#.+mysql-type-datetime+ #x00)))
(define-test binary-encoding-datetime-4 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-date-time :year 1900 :month 01 :day 01 :hour 12 :microsecond 1)
   #(#x0b #x6c #x07 #x01 #x01 #x0c #x00 #x00 #x01 #x00 #x00 #x00)
   #(#.+mysql-type-datetime+ #x00)))
;; Time Intervals, at lengths 0, 8, and 12
(define-test binary-encoding-time-1 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-time-interval)
   #(#x00)
   #(#.+mysql-type-time+ #x00)))
(define-test binary-encoding-time-2 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-time-interval :days 1 :hours 2 :minutes 3 :seconds 4)
   #(#x08 #x00 #x01 #x00 #x00 #x00 #x02 #x03 #x04)
   #(#.+mysql-type-time+ #x00)))
(define-test binary-encoding-time-3 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-time-interval :microseconds 1)
   #(#x0c #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00)
   #(#.+mysql-type-time+ #x00)))
(define-test binary-encoding-time-4 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-time-interval :microseconds 1 :negativep t)
   #(#x0c #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01 #x00 #x00 #x00)
   #(#.+mysql-type-time+ #x00)))
(define-test binary-encoding-time-5 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-time-interval :negativep t :days 1 :hours 2 :minutes 3 :seconds 4)
   #(#x08 #x01 #x01 #x00 #x00 #x00 #x02 #x03 #x04)
   #(#.+mysql-type-time+ #x00)))
;; Years, which travel like shorts
(define-test binary-encoding-year-1 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-year)
   #(#x00 #x00)
   #(#.+mysql-type-year+ #x00)))
(define-test binary-encoding-year-2 ()
  (generate-binary-encoding-test
   (make-instance 'mysql-year :year 1900)
   #(#x6c #x07)
   #(#.+mysql-type-year+ #x00)))

(define-test-suite binary-encoding-date-time-suite ()
  (binary-encoding-datetime-1
   binary-encoding-datetime-2
   binary-encoding-datetime-3
   binary-encoding-datetime-4
   binary-encoding-time-1
   binary-encoding-time-2
   binary-encoding-time-3
   binary-encoding-time-4
   binary-encoding-time-5
   binary-encoding-year-1
   binary-encoding-year-2))

(define-test-suite mysql-binary-encoding-suite ()
  (binary-encoding-octets-suite
   binary-encoding-string-suite
   binary-encoding-integer-suite
   binary-encoding-ratio-suite
   binary-encoding-float-suite
   binary-encoding-date-time-suite))

(register-test 'mysql-binary-encoding-suite)
