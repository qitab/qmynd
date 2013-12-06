;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Package declaration for MySQL Native Driver tests

(defpackage :qmynd-test
  (:use :common-lisp :qmynd-impl)
  #+test-tools
  (:import-from :qtest
   #:define-test
   #:define-test-suite
   #:register-test
   #:run-test
   #:assert-equal
   #:assert-equal-helper
   #:assert-true
   #:assert-false)

  (:import-from :qmynd-impl
  ;; utilities
   #:single-float-bits
   #:double-float-bits
   #:make-single-float
   #:make-double-float
   #:encode-binary-parameter
   #:read-wire-packet

  ;; MySQL Basic Type I/O
   #:read-fixed-length-integer
   #:read-length-encoded-integer
   #:read-fixed-length-octets
   #:read-null-terminated-octets
   #:read-length-encoded-octets
   #:write-fixed-length-integer
   #:write-length-encoded-integer

  ;; MySQL Date/Time classes
   #:mysql-date-time
   #:mysql-time-interval
   #:mysql-year

  ;; MySQL Constants
   #:+mysql-type-decimal+
   #:+mysql-type-tiny+
   #:+mysql-type-short+
   #:+mysql-type-long+
   #:+mysql-type-float+
   #:+mysql-type-double+
   #:+mysql-type-null+
   #:+mysql-type-timestamp+
   #:+mysql-type-longlong+
   #:+mysql-type-int24+
   #:+mysql-type-date+
   #:+mysql-type-time+
   #:+mysql-type-datetime+
   #:+mysql-type-year+
   #:+mysql-type-newdate+
   #:+mysql-type-varchar+
   #:+mysql-type-bit+
   #:+mysql-type-newdecimal+
   #:+mysql-type-enum+
   #:+mysql-type-set+
   #:+mysql-type-tiny-blob+
   #:+mysql-type-medium-blob+
   #:+mysql-type-long-blob+
   #:+mysql-type-blob+
   #:+mysql-type-var-string+
   #:+mysql-type-string+
   #:+mysql-type-geometry+))
