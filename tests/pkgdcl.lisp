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
  (:use :common-lisp :qmynd)
  #+test-tools
  (:import-from :qtest
   :define-test
   :define-test-suite
   :register-test
   :run-test
   :assert-equal
   :assert-true
   :assert-false)
  ;; utilities
  (:import-from :qmynd
   :single-float-bits
   :double-float-bits
   :make-single-float
   :make-double-float)
  ;; MySQL Basic Type I/O
  (:import-from :qmynd
   :read-fixed-length-integer
   :read-length-encoded-integer
   :read-fixed-length-string
   :read-null-terminated-string
   :read-length-encoded-string
   :write-fixed-length-integer
   :write-length-encoded-integer))
