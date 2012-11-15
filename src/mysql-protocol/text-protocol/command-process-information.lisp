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
;;; 15.6.11 command-process-info
;;; NB: This command is deprecated; use the query "SHOW PROCESSLIST" instead.

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-process-info
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-process-info+
;;         :transient t :bind nil)))

;; Returns Resultset or ERR packet

(defun send-command-process-information ()
  (mysql-command-init +mysql-command-process-information+)
  (mysql-write-packet (vector +mysql-command-process-information+))
  (let* ((payload (mysql-read-packet))
         (tag (aref payload 0)))
    (if (= tag +mysql-response-error+)
        (parse-response payload)
        (let* ((column-count (parse-column-count payload))
               (column-definitions (coerce
                                    (loop
                                      repeat column-count
                                      collect (parse-column-definition-v41 (mysql-read-packet))
                                      ;; Consume the EOF packet, or signal an error for an ERR packet.
                                      finally (parse-response (mysql-read-packet)))
                                    'vector))
               (rows (parse-resultset-rows column-count column-definitions)))
          (values rows column-definitions)))))

#|
;; If COM_PROCESS_INFO ever gets disabled, compatability stub:
;; NB: this file will need to depend on command-process-query
(defun send-command-process-information ()
  (send-command-query "SHOW PROCESSLIST"))
|#
