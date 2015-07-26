;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage qmynd
  (:use)

  (:export
   ;; Conditions / Errors
   #:mysql-base-error
   #:mysql-error
   #:mysql-insufficient-capabilities
   #:mysql-unsupported-authentication
   #:unexpected-packet
   #:value-is-not-decimal
   #:ssl-not-supported

   ;; Constants
   #:+mysql-server-status-in-transaction+
   #:+mysql-server-status-autocommit+
   #:+mysql-server-more-results-exist+
   #:+mysql-server-status-no-good-index-used+
   #:+mysql-server-status-no-index-used+
   #:+mysql-server-status-cursor-exists+
   #:+mysql-server-server-status-last-row-sent+
   #:+mysql-server-status-database-dropped+
   #:+mysql-server-status-no-backslash-escapes+
   #:+mysql-server-status-metadata-changed+
   #:+mysql-server-query-was-slow+
   #:+mysql-server-ps-out-params+

   ;; Opaque Structures
   #:mysql-base-connection
   #:mysql-prepared-statement

   ;; Transparent Structures
   #:mysql-date-time
   #:mysql-date-time-year
   #:mysql-date-time-month
   #:mysql-date-time-day
   #:mysql-date-time-hour
   #:mysql-date-time-minute
   #:mysql-date-time-second
   #:mysql-date-time-microsecond
   #:mysql-date-time-to-universal-time
   #:universal-time-to-mysql-date-time
   #:mysql-time-interval
   #:mysql-time-interval-negativep
   #:mysql-time-interval-days
   #:mysql-time-interval-hours
   #:mysql-time-interval-minutes
   #:mysql-time-interval-seconds
   #:mysql-time-interval-microseconds
   #:mysql-time-interval-to-seconds
   #:seconds-to-mysql-time-interval
   #:mysql-year
   #:mysql-year-year

   ;; API
   ;; Dynamic bindings
   #:*mysql-encoding*
   ;; Connections
   #:mysql-connect
   #+(or ccl sbcl ecl)
   #:mysql-local-connect
   #:mysql-disconnect
   #:mysql-connection-has-status
   #:mysql-connection-has-capability
   ;; Basic Commands
   #:mysql-query
   ;; Basic Response Packet Types / Accessors
   #:response-ok-packet
   #:response-ok-packet-affected-rows
   #:response-ok-packet-last-insert-id
   #:response-ok-packet-status-flags
   #:response-ok-packet-warnings
   ;; Prepared Statements
   #:mysql-statement-prepare
   #:mysql-statement-execute
   #:mysql-statement-close))

(defpackage qmynd-impl
  (:use :common-lisp :list-of :qmynd)
  (:import-from :uiop
   #:strcat
   #:string-prefix-p)

  (:export
   ;; Dynamic Variables
   #:*mysql-connection*
   #:*mysql-encoding*

   ;; Commands
   #:send-command-debug
   #:send-command-field-list
   #:send-command-initialize-database
   #:send-command-ping
   #:send-command-process-information
   #:send-command-process-kill
   #:send-command-query
   #:send-command-quit
   #:send-command-refresh
   #:send-command-shutdown
   #:send-command-statement-close
   #:send-command-statement-execute
   #:parse-command-statement-execute-response
   #:send-command-statement-prepare
   #:send-command-statement-reset
   #:send-command-statement-send-long-data
   #:send-command-statistics

   ;; Internal Helpers
   #:with-mysql-connection
   #:mysql-command-init
   #:mysql-has-capability
   #:mysql-has-some-capability
   #:mysql-connection-has-some-capability
   #:mysql-connection-stream
   #:mysql-current-command-p
   #:flagsp

   ;; Columnd defintion stuff
   #:column-definition-encoding
   #:column-definition-type

   ;; Resultset Parser
   #:parse-response
   #:parse-column-count
   #:parse-resultset-rows
   #:parse-text-protocol-result-column
   #:parse-binary-resultset-rows
   #:parse-binary-protocol-result-column

   ;; Prepared Statement Protocol
   #:encode-binary-parameter

   ;; Wire Protocol / Low-level connection
   #:mysql-read-packet
   #:mysql-write-packet
   #:mysql-connection-read-packet
   #:mysql-connection-write-packet

   ;; Initial Handshake:
   #:process-initial-handshake-payload
   #:process-initial-handshake-v10
   #:send-handshake-response-41

   ;; Auth Stuff
   #:generate-auth-response
   #+mysql-insecure-password-hash #:mysql-weak-hash-password
   #:mysql-native-password-auth-response
   #:mysql-clear-password-auth-response

   ;; Packet Definition Macrology and Related Functions
   #:define-packet
   #:packet-slot
   #:packet-slot-bind
   #:packet-slot-eof
   #:packet-slot-predicate
   #:packet-slot-reduce
   #:packet-slot-transform
   #:packet-slot-transient
   #:packet-slot-mysql-type
   #:packet-slot-type
   #:packet-slot-value
   #:parse-slot
   #:emit-packet-parser
   #:emit-packet-parser-slot
   #:emit-packet-parser-slot-reader
   #:emit-packet-slot-lisp-type
   #:emit-packet-struct

   ;; Accessors for user-opaque structures
   #:mysql-prepared-statement-connection
   #:mysql-prepared-statement-query-string
   #:mysql-prepared-statement-statement-id
   #:mysql-prepared-statement-columns
   #:mysql-prepared-statement-parameters
   #:mysql-connection-connected
   #:mysql-connection-socket
   #:mysql-connection-server-version
   #:mysql-connection-connection-id
   #:mysql-connection-capabilities
   #:mysql-connection-character-set
   #:mysql-connection-cs-coll
   #:mysql-connection-status-flags
   #:mysql-connection-sequence-id
   #:mysql-connection-auth-data
   #:mysql-connection-auth-plugin
   #:mysql-connection-default-schema
   #:mysql-connection-current-command
   #:mysql-connection-prepared-statements))
