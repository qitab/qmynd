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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.6.9 command-shutdown

;; We don't actually receive this packet as a client, but it looks like this.
;; (define-packet command-shutdown
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-shutdown+
;;         :transient t :bind nil)
;;    ;; asedeno-TODO: add a default value to define-packet?
;;    (flags :mysql-type (integer 1) :eof :end)))

;; Returns EOF or ERR packet

(defun send-command-shutdown (&optional shutdown-type)
  (assert (typep shutdown-type '(or (unsigned-byte 8) null)))
  (mysql-command-init +mysql-command-shutdown+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-shutdown+ s)
     (when (and shutdown-type (not (= shutdown-type +mysql-shutdown-default+)))
       (write-byte shutdown-type s))))
  (parse-response (mysql-read-packet)))
