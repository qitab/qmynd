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
;;; 15.6.14 command-debug

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-debug
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-debug+
;;         :transient t :bind nil)))

;; Returns EOF or ERR packet

(defun send-command-debug ()
  (mysql-command-init +mysql-command-debug+)
  (mysql-write-packet (vector +mysql-command-debug+))
  (parse-response (mysql-read-packet)))
