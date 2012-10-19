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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.7.8 command-statement-reset

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-statement-reset
;;   ((tag :mysql-type (integer 1) :value +mysql-command-statement-reset+ :transient t :bind nil)
;;    (statement-id :mysql-type (integer 4))))

;; Returns OK or ERR packet

(defun send-command-statement-reset (statement-id)
  (assert (typep statement-id '(unsigned-byte 32)))
  (with-mysql-connection (c)
    (mysql-command-init c +mysql-command-statement-reset+)
    (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
      (write-byte +mysql-command-statement-reset+ s)
      (write-fixed-length-integer statement-id 4 s)
      (mysql-write-packet (flexi-streams:get-output-stream-sequence s)))
    (parse-response (mysql-read-packet))))
