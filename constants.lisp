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

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; MySQL Commands
;; Text Protocol (15.6)
(defconstant $mysql-command-sleep #x00) ;MySQL Internal
(defconstant $mysql-command-quit #x01)
(defconstant $mysql-command-initialize-database #x02)
(defconstant $mysql-command-query #x03)
(defconstant $mysql-command-field-list #x04)
(defconstant $mysql-command-create-database #x05)
(defconstant $mysql-command-drop-database #x06)
(defconstant $mysql-command-refresh #x07)
(defconstant $mysql-command-shutdown #x08)
(defconstant $mysql-command-statistics #x09)
(defconstant $mysql-command-process-information #x0a) ; deprecated (5.16.11)
(defconstant $mysql-command-connect #x0b) ;MySQL Internal
(defconstant $mysql-command-kill #x0c)
(defconstant $mysql-command-debug #x0d) ;Requires SUPER priviledge
(defconstant $mysql-command-ping #x0e)
(defconstant $mysql-command-time #x0f) ;MySQL Internal
(defconstant $mysql-command-delayed-insert #x10) ;MySQL Internal
(defconstant $mysql-command-change-user #x11)
(defconstant $mysql-command-daemon #x1d) ;MySQL Internal

;; Prepared Statements
(defconstant $mysql-command-statement-prepare #x16)
(defconstant $mysql-command-statement-execute #x17)
(defconstant $mysql-command-statement-send-long-data #x18)
(defconstant $mysql-command-statement-close #x19)
(defconstant $mysql-command-statement-reset #x1a)

;; Stored Procedures
(defconstant $mysql-command-set-option #x1b)
(defconstant $mysql-command-statement-fetch #x1c)

;; Replication Protocol (not supported by this library)
(defconstant $mysql-command-binary-log-dump #x12)
(defconstant $mysql-command-table-dump #x13)
(defconstant $mysql-command-connect-out #x14)
(defconstant $mysql-command-register-slave #x15)
(defconstant $mysql-command-binary-log-dump-gtid #x1e)

;; Shutdown types (15.6.9)
;; NB: Only $mysql-shutdown-wait-all-buffers is used
(defconstant $mysql-shutdown-default #x00)
(defconstant $mysql-shutdown-wait-connections #x01)
(defconstant $mysql-shutdown-wait-transactions #x02)
(defconstant $mysql-shutdown-wait-updates #x08)
(defconstant $mysql-shutdown-wait-all-buffers #x10)
(defconstant $mysql-shutdown-wait-critical-buffers #x11)
(defconstant $mysql-shutdown-kill-query #xfe)
(defconstant $mysql-shutdown-kill-connection #xff)


) ;eval-when