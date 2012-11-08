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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.6.10 command-statistics

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-statistics
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-statistics+
;;         :transient t :bind nil)))

;; Returns (string :eof)

(defun send-command-statistics ()
  (mysql-command-init +mysql-command-statistics+)
  (mysql-write-packet (vector +mysql-command-statistics+))
  (babel:octets-to-string (mysql-read-packet)))
