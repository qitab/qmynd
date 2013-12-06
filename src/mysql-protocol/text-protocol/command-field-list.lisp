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
;;; 15.6.5 command-field-list

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-field-list
;;   ((tag :mysql-type (integer 1)
;;         :value +mysql-command-field-list+
;;         :transient t :bind nil)
;;    (table :mysql-type (string :null))
;;    (field-wildcard :mysql-type (string :eof))))

(defun send-command-field-list (table &optional field-wildcard)
  (mysql-command-init +mysql-command-field-list+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-field-list+ s)
     (write-null-terminated-octets (babel:string-to-octets table) s)
     (when field-wildcard
       (write-sequence (babel:string-to-octets field-wildcard) s))))
  (let* ((initial-payload (mysql-read-packet))
         (tag (aref initial-payload 0)))
    (if (= tag +mysql-response-error+)
        (parse-response initial-payload)
        (coerce
         (loop
           for payload = initial-payload then (mysql-read-packet)
           until (and (= (aref payload 0) +mysql-response-end-of-file+)
                      (< (length payload) 9))
           collect (parse-column-definition-v41 payload)
           ;; Consume the EOF packet or signal an error for an ERR packet.
           finally (parse-response payload))
         'vector))))
