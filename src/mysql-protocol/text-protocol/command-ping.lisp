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
;;; 15.6.15 command-ping

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-ping
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-ping+
;;         :transient t :bind nil)))

;; Returns OK packet

(defun send-command-ping ()
  (mysql-command-init +mysql-command-ping+)
  (mysql-write-packet (vector +mysql-command-ping+))
  (parse-response (mysql-read-packet)))
