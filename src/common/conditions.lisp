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

(define-condition mysql-base-error (error)
  ())

(define-condition mysql-error (mysql-base-error)
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

(define-condition mysql-insufficient-capabilities (mysql-base-error)
  ((server-flags :initarg server-flags
                 :reader mysql-insufficient-capabilities-server-flags)))

(define-condition mysql-unsupported-authentication (mysql-base-error)
  ((plugin :initarg :plugin
           :reader mysql-unsupported-authentication-plugin)))

(define-condition unexpected-packet (mysql-base-error)
  ((payload :initarg :payload
            :reader unexpected-packet-payload)))

(define-condition bad-mysql-type-spec (mysql-error)
  ((text :initarg :text
         :reader bad-mysql-type-spec-text)))
