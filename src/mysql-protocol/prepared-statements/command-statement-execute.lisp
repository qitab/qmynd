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

(defun send-command-statement-execute (statement &key parameters)
  ;; asedeno-TODO: signal new condition here.
  (assert (member (length parameters)
                  (list 0 (length (mysql-prepared-statement-parameters statement)))
                  :test #'=))
  (with-mysql-connection (c)
    (mysql-command-init c +mysql-command-statement-execute+)
    (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
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
          (loop
            for i from 0
            for parameter across parameters
            if parameter
              do (encode-binary-parameter parameter parameter-stream parameter-type-stream)
            else
              do (setf (ldb (byte 1 i) null-bitmap) 1)
            end
            finally (write-fixed-length-integer null-bitmap (ceiling i 8) s)
                    (let ((types (flexi-streams:get-output-stream-sequence parameter-type-stream)))
                      (write-byte (if (zerop (length types)) 0 1) s)
                      (write-sequence types s)
                      (write-sequence (flexi-streams:get-output-stream-sequence parameter-stream) s)))))
      (mysql-write-packet (flexi-streams:get-output-stream-sequence s)))
    (parse-command-statement-execute-response statement)))

(defun parse-command-statement-execute-response (statement)
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
                                     ;; Consume the EOF packet or signal an error for an ERR packet.
                                     finally (parse-response (mysql-read-packet)))
                                   'vector))
              (rows (parse-binary-resultset-rows column-count column-definitions)))
         ;; The column definitions may have changed.
         (setf (mysql-prepared-statement-columns statement) column-definitions)
         (values rows column-definitions))))))

