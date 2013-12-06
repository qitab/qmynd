;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-test)


;;; Ultra light-weight test framework

(defmacro define-test (test-name () &body body)
  `(defun ,test-name ()
     (handler-case
         (progn ,@body)
       (error (e)
         (warn "An error was signalled executing ~S:~% ~A"
               ',test-name e)))))

(defmacro define-test-suite (suite-name () &body body)
  (if (listp (car body))
    ;; QRes-style body
    `(defun ,suite-name ()
       ,@(loop for test in (car body)
               collect (list test)))
    ;; The more sensible style
    `(defun ,suite-name ()
       ,@(loop for test in body
               collect (list test)))))

(defvar *all-registered-tests* ())
(defmacro register-test (test-name)
  `(pushnew ,test-name *all-registered-tests*))

(defmacro run-test (test-name)
  `(progn
     (format t "~&Running test ~A" ',test-name)
     (funcall ',test-name)))

(defun run-all-tests ()
  (dolist (test *all-registered-tests*)
    (format t "~&Running test ~A" test)
    (funcall test)))

(defmacro assert-equal (actual expected &key (test ''equal))
  `(assert-equal-helper ',actual ,actual ',expected ,expected ,test))

(defun assert-equal-helper (actual-form actual expected-form expected test)
  (unless (funcall test actual expected)
     (warn "These two expressions yield values that are not ~S:~% ~S => ~S~%~S => ~S"
           test actual-form actual expected-form expected)))

(defmacro assert-true (form)
  `(unless ,form
     (warn "The value ~S does not evaluate to 'true'"
           ',form)))

(defmacro assert-false (form)
  `(when ,form
     (warn "The value ~S does not evaluate to 'false'"
           ',form)))
