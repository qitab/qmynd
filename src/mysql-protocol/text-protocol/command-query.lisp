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
;;; 15.6.4 command-query

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-query
;;   ((tag :mysql-type (integer 1) :value +mysql-command-query+ :transient t :bind nil)
;;    (query-string :mysql-type (string :eof))))

(defun send-command-query (query-string)
  (mysql-command-init +mysql-command-query+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-query+ s)
     (write-sequence (babel:string-to-octets query-string) s)))
  (let* ((payload (mysql-read-packet))
         (tag (aref payload 0)))
    (if (member tag (list +mysql-response-ok+ +mysql-response-error+))
        (parse-response payload)
        (let* ((column-count (parse-column-count payload))
               (column-definitions (coerce
                                    (loop
                                      repeat column-count
                                      collect (parse-column-definition-v41 (mysql-read-packet))
                                      ;; Consume the EOF packet or signal an error for an ERR packet.
                                      finally (parse-response (mysql-read-packet)))
                                    'vector))
               (rows (parse-resultset-rows column-count column-definitions)))
          (values rows column-definitions)))))
