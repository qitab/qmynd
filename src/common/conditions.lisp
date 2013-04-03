;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library Base Conditions
(define-condition mysql-base-error (error)
  ())

(define-condition mysql-internal-error (mysql-base-error)
  ())

(define-condition mysql-external-error (mysql-base-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library Internal Errors
(define-condition bad-mysql-type-spec (mysql-internal-error)
  ((text :initarg :text
         :reader bad-mysql-type-spec-text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exported Errors

(define-condition mysql-error (mysql-external-error)
  ((code :initarg :code
         :reader mysql-error-code)
   (message :initarg :message
            :reader mysql-error-message)
   (state :initarg :state
          :reader mysql-error-state))
  (:report (lambda (e s)
             (format s "MySQL Error [~A]: \"~A\""
                     (mysql-error-code e)
                     (mysql-error-message e)))))

(define-condition ssl-not-supported (mysql-external-error)
  ())

(define-condition invalid-prepared-statement (mysql-external-error)
  ())

(define-condition unexpected-parameter-count (mysql-external-error)
  ())

(define-condition mysql-insufficient-capabilities (mysql-external-error)
  ((server-flags :initarg server-flags
                 :reader mysql-insufficient-capabilities-server-flags)))

(define-condition mysql-unsupported-authentication (mysql-external-error)
  ((plugin :initarg :plugin
           :reader mysql-unsupported-authentication-plugin)))

(define-condition unexpected-packet (mysql-external-error)
  ((payload :initarg :payload
            :reader unexpected-packet-payload)))

(define-condition value-is-not-decimal (mysql-external-error)
  ((value :initarg :value)))

