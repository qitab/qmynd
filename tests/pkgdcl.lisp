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

(defpackage :mysqlnd-test
  (:use :common-lisp :mysqlnd)
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
  (:import-from :mysqlnd
   :single-float-bits
   :double-float-bits
   :make-single-float
   :make-double-float)
  ;; mysql parsing
  (:import-from :mysqlnd
   :parse-fixed-length-integer
   :parse-length-encoded-integer
   :parse-fixed-length-string
   :parse-null-terminated-string
   :parse-length-encoded-string)
  ;; mysql encoding
  (:import-from :mysqlnd
   :encode-fixed-length-integer
   :encode-length-encoded-integer))
