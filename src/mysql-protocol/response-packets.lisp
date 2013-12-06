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

;;; 15.1.3. Generic Response Packets

(define-packet response-ok
    ((tag :mysql-type (integer 1) :value +mysql-response-ok+ :transient t :bind nil)
     (affected-rows :mysql-type (integer :lenenc))
     (last-insert-id :mysql-type (integer :lenenc))
     (status-flags :mysql-type (integer 2)
                   :predicate (mysql-has-some-capability
                               #.(logior +mysql-capability-client-protocol-41+
                                         +mysql-capability-client-transactions+)))
     (warnings :mysql-type (integer 2)
               :predicate (mysql-has-capability +mysql-capability-client-protocol-41+))
     (info :mysql-type (string :eof))))

(define-packet response-error
    ((tag :mysql-type (integer 1) :value +mysql-response-error+ :transient t :bind nil)
     (error-code :mysql-type (integer 2))
     ;; This really a string, but we're just checking to see it's a #\#
     (state-marker :mysql-type (integer 1)
                   :predicate (mysql-has-capability +mysql-capability-client-protocol-41+)
                   :value #.(char-code #\#)
                   :transient t
                   :bind nil)
     (sql-state :mysql-type (string 5)
                :predicate (mysql-has-capability +mysql-capability-client-protocol-41+))
     (error-message :mysql-type (string :eof))))

(define-packet response-end-of-file
    ((tag :mysql-type (integer 1) :value +mysql-response-end-of-file+ :transient t :bind nil)
     (warning-count :mysql-type (integer 2)
                    :predicate (mysql-has-capability +mysql-capability-client-protocol-41+))
     (status-flags :mysql-type (integer 2)
                   :predicate (mysql-has-capability +mysql-capability-client-protocol-41+))))

(defun parse-response (stream)
  "Parse a generic (OK, ERR, EOF) packet.
   Update MySQL connection status flags as necessary."
  (declare (type my-packet-stream stream))
  (let ((tag (peek-first-octet stream)))
    (cond
      ((= tag +mysql-response-ok+)
       (let ((packet (parse-response-ok stream)))
         (setf (mysql-connection-status-flags *mysql-connection*)
               (response-ok-packet-status-flags packet))
         packet))
      ((= tag +mysql-response-error+)
       (let ((packet (parse-response-error stream)))
         (error (make-condition 'mysql-error
                                :code    (response-error-packet-error-code packet)
                                :message (response-error-packet-error-message packet)
                                :state   (response-error-packet-sql-state packet)))))
      ((= tag +mysql-response-end-of-file+)
       (let ((packet (parse-response-end-of-file stream)))
         (setf (mysql-connection-status-flags *mysql-connection*)
               (response-end-of-file-packet-status-flags packet))
         packet))
      (t
       (error (make-condition 'unexpected-packet :stream stream))))))
