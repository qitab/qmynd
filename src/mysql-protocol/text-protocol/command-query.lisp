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
;;; 15.6.4 command-query

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-query
;;   ((tag :mysql-type (integer 1) :value +mysql-command-query+ :transient t :bind nil)
;;    (query-string :mysql-type (string :eof))))

(defun send-command-query (query-string)
  (with-mysql-connection (c)
    (mysql-command-init c +mysql-command-query+)
    (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
      (write-byte +mysql-command-query+ s)
      (write-sequence (babel:string-to-octets query-string) s)
      (mysql-write-packet (flexi-streams:get-output-stream-sequence s)))
    (let* ((payload (mysql-read-packet))
           (tag (aref payload 0)))
      (case tag
        ((#.+mysql-response-ok+ #.+mysql-response-error+) (parse-response payload))
        (otherwise
         (let* ((column-count (parse-column-count payload))
                (column-definitions (coerce
                                     (loop
                                       repeat column-count
                                       collect (parse-column-definition-v41 (mysql-read-packet))
                                       ;; asedeno-TODO: do something better here; probably signal for
                                       ;; ERR in parse-response.
                                       finally (assert (typep (parse-response (mysql-read-packet))
                                                              'response-end-of-file-packet)))
                                     'vector))
                (rows (parse-resultset-rows column-count column-definitions)))
           (values rows column-definitions)))))))
