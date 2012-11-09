;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)

(defun mysql-statement-prepare (connection query-string)
  (bind-mysql-connection (connection)
    (send-command-statement-prepare query-string)))

(defmethod mysql-statement-execute ((statement mysql-prepared-statement) &key parameters)
  (with-prefixed-accessors (connection statement-id)
      (mysql-prepared-statement- statement)
    ;;; asedeno-TODO: signal something real here
    (assert connection)
    (bind-mysql-connection (connection)
      (send-command-statement-execute statement :parameters parameters))))

(defmethod mysql-statement-reset ((statement mysql-prepared-statement))
  (with-prefixed-accessors (connection statement-id)
      (mysql-prepared-statement- statement)
    ;;; asedeno-TODO: signal something real here
    (assert connection)
    (bind-mysql-connection (connection)
      (send-command-statement-reset statement)
      (values))))

(defmethod mysql-statement-close ((statement mysql-prepared-statement))
  (with-prefixed-accessors (connection statement-id)
      (mysql-prepared-statement- statement)
    ;;; asedeno-TODO: signal something real here
    (assert connection)
    (bind-mysql-connection (connection)
      (send-command-statement-close statement)
      (mysql-connection-remove-stale-prepared-statements *mysql-connection*)
      (values))))
