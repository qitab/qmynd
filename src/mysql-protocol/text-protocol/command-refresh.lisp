;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.6.8 command-refresh

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-refresh
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-refresh+
;;         :transient t :bind nil)
;;    (flags :mysql-type (integer 1))))

;; Returns OK or ERR packet

(defun send-command-refresh (flags)
  (assert (typep flags '(unsigned-byte 8)))
  (with-mysql-connection (c)
    (mysql-command-init c +mysql-command-refresh+)
    (mysql-write-packet (vector +mysql-command-refresh+ flags))
    (parse-response (mysql-read-packet))))
