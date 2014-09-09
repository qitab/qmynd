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
  ()
  (:documentation "Base class for all QMyND MySQL errors."))

(define-condition mysql-internal-error (mysql-base-error)
  ()
  (:documentation "Base class for all QMyND internal errors."))

(define-condition mysql-external-error (mysql-base-error)
  ()
  (:documentation "Base class for all QMyND MySQL external (exported) errors."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library Internal Errors
(define-condition bad-mysql-type-spec (mysql-internal-error)
  ((text :initarg :text
         :reader bad-mysql-type-spec-text))
  (:documentation "Signaled when we encounter a bad type spec in DEFINE-PACKET."))

(define-condition invalid-length-encoded-integer (mysql-internal-error)
  ((text :initarg :text
         :reader invalid-length-encoded-integer-text)))

(define-condition unexpected-sequence-id (mysql-internal-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exported Errors

(define-condition mysql-error (mysql-external-error)
  ((code :initarg :code
         :reader mysql-error-code)
   (message :initarg :message
            :reader mysql-error-message)
   (state :initarg :state
          :reader mysql-error-state))
  (:documentation "Wraps errors returned in MySQL Error Packets.")
  (:report (lambda (e s)
             (format s "MySQL Error [~A]: \"~A\""
                     (mysql-error-code e)
                     (mysql-error-message e)))))

(define-condition ssl-not-supported (mysql-external-error)
  ()
  (:documentation "Signaled when SSL is requested but not supported."))

(define-condition invalid-prepared-statement (mysql-external-error)
  ()
  (:documentation "Signaled when trying to use an invalid MYSQL-PREPARED-STATEMENT."))

(define-condition unexpected-parameter-count (mysql-external-error)
  ()
  (:documentation "Signaled when the wrong number of parameters are passed while calling #'MYSQL-STATEMENT-EXECUTE."))

(define-condition mysql-insufficient-capabilities (mysql-external-error)
  ((server-flags :initarg server-flags
                 :reader mysql-insufficient-capabilities-server-flags))
  (:documentation "Signaled when connecting to a server that does not meet the minimum requirements for the library."))

(define-condition mysql-unsupported-authentication (mysql-external-error)
  ((plugin :initarg :plugin
           :reader mysql-unsupported-authentication-plugin))
  (:documentation "Signaled when trying to authenticate to a server with an unsupported authentication plugin."))

(define-condition unexpected-packet (mysql-external-error)
  ((payload :initarg :payload
            :reader unexpected-packet-payload))
  (:documentation "Signaled when the library encounters an unexpected packet."))

(define-condition value-is-not-decimal (mysql-external-error)
  ((value :initarg :value))
  (:documentation "Signaled when calling #'WRITE-DECIMAL-TO-STRING with a value that is not a decimal."))

(define-condition protocol-version-mismatch (mysql-external-error)
  ((version :initarg :version))
  (:documentation "Signaled when the initial handshake returns an unknown protocol value."))
