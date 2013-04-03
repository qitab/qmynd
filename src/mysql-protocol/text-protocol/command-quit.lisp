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
;;; 15.6.2 command-quit

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-quit
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-quit+
;;         :transient t :bind nil)))

;; Closes connection or returns OK packet.

(defun send-command-quit ()
  (mysql-command-init +mysql-command-quit+)
  (mysql-write-packet (vector +mysql-command-quit+))
  ;; Don't bother listening for the OK packet, just close the connection.
  (mysql-connection-close-socket *mysql-connection*))
