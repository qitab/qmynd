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
;;; 15.7.7 command-statement-close

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-statement-close
;;   ((tag :mysql-type (integer 1) :value +mysql-command-statement-close+ :transient t :bind nil)
;;    (statement-id :mysql-type (integer 4))))

(defun send-command-statement-close (statement-id)
  (assert (typep statement-id '(unsigned-byte 32)))
  (with-mysql-connection (c)
    (mysql-command-init c +mysql-command-statement-close+)
    (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
      (write-byte +mysql-command-statement-close+ s)
      (write-fixed-length-integer statement-id 4 s)
      (mysql-write-packet (flexi-streams:get-output-stream-sequence s)))
    ;; No response from server
    (values)))
