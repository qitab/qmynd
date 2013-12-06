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
;;; 15.6.18 command-change-user

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-change-user
;;   ((tag :mysql-type (integer 1) :value +mysql-command-change-user+ :transient t :bind nil)
;;    (username :mysql-type (string :null))
;;    (auth-response-length
;;     :mysql-type (integer 1)
;;     :predicate (mysql-has-capability +mysql-capability-client-secure-connection+)
;;     :transient t)
;;    (auth-response
;;     :mysql-type (string auth-response-length)
;;     :predicate (mysql-has-capability +mysql-capability-client-secure-connection+))
;;    ;; This appears twice with different predicates.
;;    (auth-response
;;     :mysql-type (string :null)
;;     :predicate (not (mysql-has-capability +mysql-capability-client-secure-connection+)))
;;    (schema :mysql-type (string :null))
;;    (character-set :mysql-type (integer 2) :eof :end)
;;    (auth-plugin :mysql-type (string :null) :eof :end)))

(defun send-command-change-user (&key (username "") (password "") (schema "" schemap))
  (flet ((reset-state ()
           ;; Sending this command resets all prepared statements, so mark all known prepared statements as
           ;; invalid and drop them from the connection state.
           (mapc #'(lambda (ps) (setf (mysql-prepared-statement-connection ps) nil))
                 (mysql-connection-prepared-statements *mysql-connection*))
           (setf (mysql-connection-prepared-statements *mysql-connection*) nil)))

    (mysql-command-init +mysql-command-change-user+)
    (with-prefixed-accessors (auth-data auth-plugin)
        (mysql-connection- *mysql-connection*)
      (mysql-write-packet
       (flexi-streams:with-output-to-sequence (s)
         (let ((auth-response (generate-auth-response password auth-data auth-plugin)))
           (write-byte +mysql-command-change-user+ s)
           (write-null-terminated-octets (babel:string-to-octets username) s)
           (cond
             ((mysql-has-capability +mysql-capability-client-secure-connection+)
              (let ((auth-response-length (length auth-response)))
                (assert (<= auth-response-length 255))
                (write-byte auth-response-length s)
                (write-sequence auth-response s)))
             (t
              (write-null-terminated-octets auth-response s)))
           (write-null-terminated-octets (babel:string-to-octets schema) s)
           ;; Requires +mysql-capability-client-protocol-41+, which this library assumes is always set,
           (write-fixed-length-integer (mysql-connection-cs-coll *mysql-connection*) 2 s)
           (when (mysql-has-capability +mysql-capability-client-plugin-auth+)
             (write-null-terminated-octets (babel:string-to-octets auth-plugin) s)))))
      ;; Once the packet has been sent, do whatever state resetting we need to do.
      (reset-state)
      ;; asedeno-TODO: When we finally support +mysql-capability-client-plugin-auth+, we'll need to
      ;; deal with a possible Authentication Method Switch Response.
      )
    (prog1
        (parse-response (mysql-read-packet))
      (setf (mysql-connection-default-schema *mysql-connection*) (when schemap schema))
      (values))))
