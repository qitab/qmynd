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
;;; 15.6.3 command-initialize-database -- change the default schema

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-initialize-database
;;   ((tag :mysql-type (integer 1) :value +mysql-command-initialize-database+ :transient t :bind nil)
;;    (schema-name :mysql-type (string :eof))))

;; Returns OK or ERR packet

(defun send-command-initialize-database (schema-name)
  (mysql-command-init +mysql-command-initialize-database+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-initialize-database+ s)
     (write-sequence (babel:string-to-octets schema-name) s)))
  (prog1
      (parse-response (mysql-read-packet))
    (setf (mysql-connection-default-schema *mysql-connection*) schema-name)))
