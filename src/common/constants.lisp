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

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; MySQL Commands
;; Text Protocol (15.6)
(defconstant +mysql-command-sleep+ #x00) ;MySQL Internal
(defconstant +mysql-command-quit+ #x01)
(defconstant +mysql-command-initialize-database+ #x02)
(defconstant +mysql-command-query+ #x03)
(defconstant +mysql-command-field-list+ #x04)
(defconstant +mysql-command-create-database+ #x05) ; deprecated
(defconstant +mysql-command-drop-database+ #x06) ; deprecated
(defconstant +mysql-command-refresh+ #x07)
(defconstant +mysql-command-shutdown+ #x08)
(defconstant +mysql-command-statistics+ #x09)
(defconstant +mysql-command-process-information+ #x0a) ; deprecated (5.16.11)
(defconstant +mysql-command-connect+ #x0b) ;MySQL Internal
(defconstant +mysql-command-process-kill+ #x0c)
(defconstant +mysql-command-debug+ #x0d) ;Requires SUPER priviledge
(defconstant +mysql-command-ping+ #x0e)
(defconstant +mysql-command-time+ #x0f) ;MySQL Internal
(defconstant +mysql-command-delayed-insert+ #x10) ;MySQL Internal
(defconstant +mysql-command-change-user+ #x11)
(defconstant +mysql-command-daemon+ #x1d) ;MySQL Internal

;; Prepared Statements
(defconstant +mysql-command-statement-prepare+ #x16)
(defconstant +mysql-command-statement-execute+ #x17)
(defconstant +mysql-command-statement-send-long-data+ #x18)
(defconstant +mysql-command-statement-close+ #x19)
(defconstant +mysql-command-statement-reset+ #x1a)

;; Stored Procedures
(defconstant +mysql-command-set-option+ #x1b)
(defconstant +mysql-command-statement-fetch+ #x1c)

;; Replication Protocol (not supported by this library)
(defconstant +mysql-command-binary-log-dump+ #x12)
(defconstant +mysql-command-table-dump+ #x13)
(defconstant +mysql-command-connect-out+ #x14)
(defconstant +mysql-command-register-slave+ #x15)
(defconstant +mysql-command-binary-log-dump-gtid+ #x1e)


;; Response types
(defconstant +mysql-response-ok+ #x00)
(defconstant +mysql-response-end-of-file+ #xfe)
(defconstant +mysql-response-error+ #xff)


;; Shutdown types (15.6.9)
;; NB: Only +mysql-shutdown-wait-all-buffers+ is used
(defconstant +mysql-shutdown-default+ #x00)
(defconstant +mysql-shutdown-wait-connections+ #x01)
(defconstant +mysql-shutdown-wait-transactions+ #x02)
(defconstant +mysql-shutdown-wait-updates+ #x08)
(defconstant +mysql-shutdown-wait-all-buffers+ #x10)
(defconstant +mysql-shutdown-wait-critical-buffers+ #x11)
(defconstant +mysql-shutdown-kill-query+ #xfe)
(defconstant +mysql-shutdown-kill-connection+ #xff)


;; Capability Flags (15.2.6)
(defconstant +mysql-capability-client-long-password+ #x1)
(defconstant +mysql-capability-client-found-rows+ #x2)
(defconstant +mysql-capability-client-long-flag+ #x4)
(defconstant +mysql-capability-client-connect-with-db+ #x8)

(defconstant +mysql-capability-client-no-schema+ #x10)
(defconstant +mysql-capability-client-compress+ #x20)
(defconstant +mysql-capability-client-odbc+ #x40)
(defconstant +mysql-capability-client-local-files+ #x80)

(defconstant +mysql-capability-client-ignore-space+ #x100)
(defconstant +mysql-capability-client-protocol-41+ #x200)        ;; CLIENT_CHANGE_USER in v3.22; unused in v4.0
(defconstant +mysql-capability-client-interactive+ #x400)
(defconstant +mysql-capability-client-ssl+ #x800)

(defconstant +mysql-capability-client-ignore-sigpipe+ #x1000)
(defconstant +mysql-capability-client-transactions+ #x2000)      ;; Always set by server since v4.0
(defconstant +mysql-capability-client-reserved+ #x4000)          ;; CLIENT_PROTOCOL_41 in v4.1.0; deprecated v4.1.1
(defconstant +mysql-capability-client-secure-connection+ #x8000)

(defconstant +mysql-capability-client-multi-statements+ #x10000) ;; Requires CLIENT_PROTOCOL_41
(defconstant +mysql-capability-client-multi-results+ #x20000)    ;; Requires CLIENT_PROTOCOL_41
(defconstant +mysql-capability-client-ps-multi-results+ #x40000) ;; Requires CLIENT_PROTOCOL_41

;;; NB: There are no immediate plans to support these capabilities.
(defconstant +mysql-capability-client-plugin-auth+ #x80000)      ;; New in v5.5.7; Requires CLIENT_PROTOCOL_41
(defconstant +mysql-capability-client-connect-attrs+ #x100000)   ;; New in v5.6.6
(defconstant +mysql-capability-client-plugin-auth-lenec-client-data+ #x200000) ;; New in v5.6.7

(defconstant +mysql-capability-client-verify-server-cert+ #x40000000) ;; Not used; specify :SSL-VERIFY T at connect time.
(defconstant +mysql-capability-client-remember-options+ #x80000000)   ;; Not used.

(defconstant +mysql-capabilities-required+
  (logior
   +mysql-capability-client-long-flag+
   +mysql-capability-client-connect-with-db+ ; required of server; client use not required.
   +mysql-capability-client-protocol-41+
   +mysql-capability-client-transactions+
   +mysql-capability-client-secure-connection+)
  "The minimum required capabilities for this client to interop with a MySQL server.")

;; Status flags (15.1.3.1)
(defconstant +mysql-server-status-in-transaction+ #x1)
(defconstant +mysql-server-status-autocommit+ #x2)
;;; #x4 ?
(defconstant +mysql-server-more-results-exist+ #x8)

(defconstant +mysql-server-status-no-good-index-used+ #x10)
(defconstant +mysql-server-status-no-index-used+ #x20)
(defconstant +mysql-server-status-cursor-exists+ #x40)
(defconstant +mysql-server-server-status-last-row-sent+ #x80)

(defconstant +mysql-server-status-database-dropped+ #x100)
(defconstant +mysql-server-status-no-backslash-escapes+ #x200)
(defconstant +mysql-server-status-metadata-changed+ #x400)
(defconstant +mysql-server-query-was-slow+ #x800)

(defconstant +mysql-server-ps-out-params+ #x1000)

;; Column Types (15.6.4.1.1.1)
(defconstant +mysql-type-decimal+ #x00)
(defconstant +mysql-type-tiny+ #x01)
(defconstant +mysql-type-short+ #x02)
(defconstant +mysql-type-long+ #x03)
(defconstant +mysql-type-float+ #x04)
(defconstant +mysql-type-double+ #x05)
(defconstant +mysql-type-null+ #x06)
(defconstant +mysql-type-timestamp+ #x07)
(defconstant +mysql-type-longlong+ #x08)
(defconstant +mysql-type-int24+ #x09)
(defconstant +mysql-type-date+ #x0a)
(defconstant +mysql-type-time+ #x0b)
(defconstant +mysql-type-datetime+ #x0c)
(defconstant +mysql-type-year+ #x0d)
(defconstant +mysql-type-newdate+ #x0e)
(defconstant +mysql-type-varchar+ #x0f)
(defconstant +mysql-type-bit+ #x10)
(defconstant +mysql-type-newdecimal+ #xf6)
(defconstant +mysql-type-enum+ #xf7)
(defconstant +mysql-type-set+ #xf8)
(defconstant +mysql-type-tiny-blob+ #xf9)
(defconstant +mysql-type-medium-blob+ #xfa)
(defconstant +mysql-type-long-blob+ #xfb)
(defconstant +mysql-type-blob+ #xfc)
(defconstant +mysql-type-var-string+ #xfd)
(defconstant +mysql-type-string+ #xfe)
(defconstant +mysql-type-geometry+ #xff)

;; Column Description Flags
(defconstant +mysql-flag-column-non-nullable+ #x01)
(defconstant +mysql-flag-column-primary-key+ #x02)
(defconstant +mysql-flag-column-unique-key+ #x04)
(defconstant +mysql-flag-column-multiple-key+ #x08)
(defconstant +mysql-flag-column-blob+ #x10)
(defconstant +mysql-flag-column-unsigned+ #x20)
(defconstant +mysql-flag-column-zero-fill+ #x40)
(defconstant +mysql-flag-column-binary+ #x80)

;; Command Refresh flags (15.6.8)
(defconstant +mysql-refresh-grant+ #x01)
(defconstant +mysql-refresh-log+ #x02)
(defconstant +mysql-refresh-tables+ #x04)
(defconstant +mysql-refresh-hosts+ #x08)
(defconstant +mysql-refresh-status+ #x10)
(defconstant +mysql-refresh-threads+ #x20)
(defconstant +mysql-refresh-slave+ #x40)
(defconstant +mysql-refresh-master+ #x80)

;; Date-Time

(defconstant +seconds-per-minute+ 60)
(defconstant +minutes-per-hour+ 60)
(defconstant +hours-per-day+ 24)

(defconstant +mysql-minimum-compression-length+ 50)

) ;eval-when

(defun mysql-capabilities-supported ()
  "Returns the full set of capabilities supported by this client library."
  (logior
   +mysql-capabilities-required+
   +mysql-capability-client-connect-with-db+
   +mysql-capability-client-plugin-auth+
   (if (have-ssl)
       +mysql-capability-client-ssl+
       0)
   (if (have-compression)
       +mysql-capability-client-compress+
       0)
   ;;+mysql-capability-client-no-schema+
   ;;+mysql-capability-client-ignore-space+
   ;;+mysql-capability-client-multi-statements+
   ;;+mysql-capability-client-multi-results+
   ;;+mysql-capability-client-ps-multi-results+
   ;;+mysql-capability-client-connect-attrs+
   ))
