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

(defclass mysql-prepared-statement ()
  ((connection :type (or mysql-base-connection null)
               :initarg :connection
               :accessor mysql-prepared-statement-connection)
   (query-string :type string
                 :initarg :query-string
                 :accessor mysql-prepared-statement-query-string)
   (statement-id :type (unsigned-byte 32)
                 :initarg :statement-id
                 :accessor mysql-prepared-statement-statement-id)
   (columns :type (vector-of column-definition-v41-packet)
            :initarg :columns
            :accessor mysql-prepared-statement-columns)
   (parameters :type (vector-of column-definition-v41-packet)
               :initarg :parameters
               :accessor mysql-prepared-statement-parameters)))


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
  (mysql-command-init +mysql-command-statement-prepare+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-statement-prepare+ s)
     (write-sequence (babel:string-to-octets query-string) s)))
  (let* ((my-stream (mysql-read-packet))
         (tag (peek-first-octet my-stream)))
    (if (= tag +mysql-response-error+)
        (parse-response my-stream)
        (let* ((sp-ok (parse-command-statement-prepare-ok my-stream))
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
          (let ((statement
                  (make-instance 'mysql-prepared-statement
                                 :connection *mysql-connection*
                                 :query-string query-string
                                 :statement-id (command-statement-prepare-ok-packet-statement-id sp-ok)
                                 :columns columns
                                 :parameters parameters)))
            (push statement (mysql-connection-prepared-statements *mysql-connection*))
            statement)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.7.5 command-statement-send-long-data

;;; This command is used to send TEXT and BLOB data outside of command-statement-execute.

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-statement-send-long-data
;;   ((tag :mysql-type (integer 1) :value +mysql-command-statement-send-long-data+ :transient t :bind nil)
;;    (statement-id :mysql-type (integer 4))
;;    (parameter-id :mysql-type (integer 2))
;;    (data :mysql-type (octets :eof))))

;; There is no response to this command

(defun send-command-statement-send-long-data (statement parameter-id data)
  (assert (typep parameter-id '(integer 0 #xffff)))
  (let ((octets (etypecase data
                  ((vector (unsigned-byte 8)) data)
                  (string (babel:string-to-octets data)))))
    (assert (eq *mysql-connection* (mysql-prepared-statement-connection statement)))
    (mysql-command-init +mysql-command-statement-send-long-data+)
    (mysql-write-packet
     (flexi-streams:with-output-to-sequence (s)
       (write-byte +mysql-command-statement-send-long-data+ s)
       (write-fixed-length-integer (mysql-prepared-statement-statement-id statement) 4 s)
       (write-fixed-length-integer parameter-id 2 s)
       (write-sequence octets s))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.7.6 command-statement-execute

;; We don't actually receive this packet as a client, but it looks like this.
;; This packet cannot be defined ahead of time anyhow.

;; (define-packet command-statement-execute
;;   ((tag :mysql-type (integer 1) :value +mysql-command-statement-execute+ :transient t :bind nil)
;;    (statement-id :mysql-type (integer 4))
;;    (flags :mysql-type (integer 1))
;;    ;; asedeno-TODO: add default value to define-packet slots
;;    (iteration-count :mysql-type (integer 4)) ;; Always 1
;;    ;; NB: The length of the null bitmap is not defined without the context of a prepared statement.
;;    (null-bitmap :mysql-type (octets n) :predicate (plusp n))
;;    (new-parameters :mysql-type (integer 1) :predicate (plusp n))
;;    (parameter-types :mysql-type (octets (* 2 n)) :predicate (= 1 new-parameters))
;;    ;; n values encoded in binary row protocol.
;;    (values :mysql-type (octets :eof) :predicate (= 1 new-parameters))))

(defmethod send-command-statement-execute  ((statement mysql-prepared-statement) &key parameters)
  (unless (member (length parameters)
                  (list 0 (length (mysql-prepared-statement-parameters statement)))
                  :test #'=)
    (error 'unexpected-parameter-count))
  (assert (eq *mysql-connection* (mysql-prepared-statement-connection statement)))
  (mysql-command-init +mysql-command-statement-execute+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-statement-execute+ s)
     (write-fixed-length-integer (mysql-prepared-statement-statement-id statement) 4 s)
     ;; asedeno-TODO: Implement flags
     (write-byte 0 s)
     ;; Iteration count: always 1
     (write-fixed-length-integer 1 4 s)
     (unless (zerop (length parameters))
       (let ((parameters (coerce parameters 'vector))
             (parameter-type-stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
             (parameter-stream (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
             (null-bitmap 0))
         (unwind-protect
              (loop
                for i from 0
                for parameter across parameters
                if parameter
                  do (encode-binary-parameter parameter parameter-stream parameter-type-stream)
                else
                  do (setf (ldb (byte 1 i) null-bitmap) 1)
                     (write-byte +mysql-type-null+ parameter-type-stream)
                     (write-byte 0 parameter-type-stream)
                end
                finally (write-fixed-length-integer null-bitmap (ceiling i 8) s)
                        (let ((types (flexi-streams:get-output-stream-sequence parameter-type-stream)))
                          (write-byte (if (zerop (length types)) 0 1) s)
                          (write-sequence types s)
                          (write-sequence (flexi-streams:get-output-stream-sequence parameter-stream) s)))
           (when parameter-type-stream (close parameter-type-stream))
           (when parameter-stream (close parameter-stream)))))))
  (parse-command-statement-execute-response statement))

(defmethod parse-command-statement-execute-response ((statement mysql-prepared-statement))
  (let* ((my-stream (mysql-read-packet))
         (tag (peek-first-octet my-stream)))
    (if (member tag (list +mysql-response-ok+ +mysql-response-error+))
        (parse-response my-stream)
        (let* ((column-count (parse-column-count my-stream))
               (column-definitions (coerce
                                    (loop
                                      repeat column-count
                                      collect (parse-column-definition-v41 (mysql-read-packet))
                                      ;; Consume the EOF packet or signal an error for an ERR packet.
                                      finally (parse-response (mysql-read-packet)))
                                    'vector))
               (rows (parse-binary-resultset-rows column-count column-definitions)))
          ;; The column definitions may have changed.
          (setf (mysql-prepared-statement-columns statement) column-definitions)
          (values rows column-definitions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.7.7 command-statement-close

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-statement-close
;;   ((tag :mysql-type (integer 1) :value +mysql-command-statement-close+ :transient t :bind nil)
;;    (statement-id :mysql-type (integer 4))))

(defmethod send-command-statement-close ((statement mysql-prepared-statement))
  (assert (eq *mysql-connection* (mysql-prepared-statement-connection statement)))
  (mysql-command-init +mysql-command-statement-close+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-statement-close+ s)
     (write-fixed-length-integer (mysql-prepared-statement-statement-id statement) 4 s)))
  ;; No response from server
  (setf (mysql-prepared-statement-connection statement) nil)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.7.8 command-statement-reset

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-statement-reset
;;   ((tag :mysql-type (integer 1) :value +mysql-command-statement-reset+ :transient t :bind nil)
;;    (statement-id :mysql-type (integer 4))))

;; Returns OK or ERR packet

(defmethod send-command-statement-reset ((statement mysql-prepared-statement))
  (assert (eq *mysql-connection* (mysql-prepared-statement-connection statement)))
  (mysql-command-init +mysql-command-statement-reset+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-statement-reset+ s)
     (write-fixed-length-integer (mysql-prepared-statement-statement-id statement) 4 s)))
  (parse-response (mysql-read-packet)))

(asdf-finalizers:final-forms)
