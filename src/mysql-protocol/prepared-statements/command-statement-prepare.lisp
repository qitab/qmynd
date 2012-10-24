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
;;; 15.7.4.1 command-statement-prepare

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-statement-prepare
;;   ((tag :mysql-type (integer 1) :value +mysql-command-statement-prepare+ :transient t :bind nil)
;;    (query-string :mysql-type (string :eof))))

(define-packet command-statement-prepare-ok
  ((status :mysql-type (integer 1) :value 0 :transient t :bind nil)
   (statement-id :mysql-type (integer 4))
   (num-columns :mysql-type (integer 2))
   (num-params :mysql-type (integer 2))
   (reserved :mysql-type (integer 1) :transient t :bind nil)
   (warning-count :mysql-type (integer 2))))

(defun send-command-statement-prepare (query-string)
  (with-mysql-connection (c)
    (mysql-command-init c +mysql-command-statement-prepare+)
    (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
      (write-byte +mysql-command-statement-prepare+ s)
      (write-sequence (babel:string-to-octets query-string) s)
      (mysql-write-packet (flexi-streams:get-output-stream-sequence s)))
    (let* ((payload (mysql-read-packet))
           (tag (aref payload 0)))
      (case tag
        ((#.+mysql-response-error+) (parse-response payload))
        (otherwise
         (let* ((sp-ok (parse-command-statement-prepare-ok payload))
                (parameter-count (command-statement-prepare-ok-packet-num-params sp-ok))
                (column-count (command-statement-prepare-ok-packet-num-columns sp-ok))
                (parameters (coerce
                             (unless (zerop parameter-count)
                               (loop
                                 repeat parameter-count
                                 collect (parse-column-definition-v41 (mysql-read-packet))
                                 ;; Consume the EOF packet or signal an error for an ERR packet.
                                 finally (parse-response (mysql-read-packet))))
                             'vector))
                (columns (coerce
                          (unless (zerop column-count)
                            (loop
                              repeat column-count
                              collect (parse-column-definition-v41 (mysql-read-packet))
                              ;; Consume the EOF packet or signal an error for an ERR packet.
                              finally (parse-response (mysql-read-packet))))
                          'vector)))
           (make-instance 'mysql-prepared-statement
                          :connection c
                          :statement-id (command-statement-prepare-ok-packet-statement-id sp-ok)
                          :columns columns
                          :parameters parameters)))))))
