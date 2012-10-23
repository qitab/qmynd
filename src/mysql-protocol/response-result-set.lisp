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
;;; 15.6.4.1 Text Resultset
;;; Two parts:
;;; • Column Definitions
;;;   · Column Count (n)
;;;   · Column Definition Packets (n total)
;;;   · EOF Packet
;;; • Rows
;;;   · One or more ResutltsetRow packets
;;;   · EOF or ERR Packet

;;; If EOF packet status has +mysql-server-more-results-exist+ set, another ResultSet packet follows.
;;; (See +mysql-capability-client-multi-results+, +mysql-capability-client-ps-multi-results+.)

;; This packet is trivial and does not require a struct.
;; (define-packet column-count
;;  ((count :mysql-type (integer :lenenc))))

(defun parse-column-count (payload)
  (let ((s (flexi-streams:make-in-memory-input-stream payload)))
    (read-length-encoded-integer s)))

(define-packet column-definition-v41
  ((catalog :mysql-type (string :lenenc))
   (schema :mysql-type (string :lenenc))
   (table :mysql-type (string :lenenc))
   (org-table :mysql-type (string :lenenc))
   (name :mysql-type (string :lenenc))
   (org-name :mysql-type (string :lenenc))
   (len-fixed-fields :mysql-type (integer :lenenc)
                     :value #x0c
                     :transient t
                     :bind nil)
   (cs-coll :mysql-type (integer 2))
   (column-length :mysql-type (integer 4))
   (type :mysql-type (integer 1))
   (flags :mysql-type (integer 2))
   (decimals :mysql-type (integer 1))
   (filler :mysql-type (integer 2)
           :value 0
           :transient t
           :bind nil)
   (default-value :mysql-type (string :lenenc-null-ok)
                  :predicate (mysql-current-command-p +mysql-command-field-list+))))

(defmethod column-definition-type ((column-definition column-definition-v41-packet))
  (column-definition-v41-packet-type column-definition))

(defun parse-resultset-rows (column-count column-definitions)
  (flet ((parse-resultset-row ()
           (let ((payload (mysql-read-packet)))
             (cond
               ((and (= (aref payload 0) +mysql-response-end-of-file+)
                     (< (length payload) 9))
                (parse-response payload))
               (t
                (let ((s (flexi-streams:make-flexi-stream (flexi-streams:make-in-memory-input-stream payload)))
                      (row (make-array column-count :initial-element nil)))
                  (loop for i from 0 below column-count
                        if (eq (flexi-streams:peek-byte s) #xfb)
                          do (read-byte s) ; throw this byte away -- it represents a NULL column.
                        else
                          do (setf (aref row i)
                                   (parse-text-protocol-result-column
                                    (read-length-encoded-string s)
                                    (aref column-definitions i))))
                  row))))))
    (coerce (loop for row = (parse-resultset-row) then (parse-resultset-row)
                  until (typep row 'response-end-of-file-packet)
                  collect row)
            'vector)))

(defun parse-text-protocol-result-column (octets column-definition)
  (let ((column-type (column-definition-type column-definition))
        str)
    (labels ((str ()
               (unless str (setf str (babel:octets-to-string octets)))
               str)
             (parse-float (&optional (float-format 'single-float))
               ;; Look into replacing this with a library, or moving it to utilities.lisp.
               (assert (every #'(lambda (x) (or (<= #.(char-code #\0) x #.(char-code #\9))
                                                (member x (list #.(char-code #\-)
                                                                #.(char-code #\e)
                                                                #.(char-code #\.)))))
                              octets))
               (with-standard-io-syntax
                 (let ((*read-default-float-format* float-format))
                   (with-input-from-string (s (str))
                     (read s)))))
             (parse-datetime ()
               ;; Look into replacing this with a library, or moving it to utilities.lisp.
               (case (length octets)
                 (4 ; YYYY
                  (encode-universal-time
                   0 0 0 1 1
                   (parse-integer (str))
                   0))
                 (8 ; hh:mm:ss
                  (encode-universal-time
                   (parse-integer (subseq (str) 6 8))
                   (parse-integer (subseq (str) 3 5))
                   (parse-integer (subseq (str) 0 2))
                   1 1 1900 0))
                 (10 ; YYYY-MM-DD
                  (encode-universal-time
                   0 0 0
                   (parse-integer (subseq (str) 8 10))
                   (parse-integer (subseq (str) 5 7))
                   (parse-integer (subseq (str) 0 4))
                   0))
                 (19 ; YYYY-MM-DD hh:mm:ss
                  (encode-universal-time
                   (parse-integer (subseq (str) 17 19))
                   (parse-integer (subseq (str) 14 16))
                   (parse-integer (subseq (str) 11 13))
                   (parse-integer (subseq (str) 8 10))
                   (parse-integer (subseq (str) 5 7))
                   (parse-integer (subseq (str) 0 4))
                   0))))
             (parse-decimal ()
               ;; Look into replacing this with a library, or moving it to utilities.lisp.
               (assert (and
                        (let ((x (aref octets 0)))
                          (or (<= #.(char-code #\0) x #.(char-code #\9))
                              (=  x #.(char-code #\-))))
                        (every #'(lambda (x) (or (<= #.(char-code #\0) x #.(char-code #\9))
                                                 (=  x #.(char-code #\.))))
                               (subseq octets 1))
                        (< (count #.(char-code #\.) octets) 2)))
               (let ((start 0)
                     (sign 1)
                     found-decimal)
                 (when (= (aref octets 0) #.(char-code #\-))
                   (setq start 1
                         sign -1))
                 (loop
                   for i from start below (length (str))
                   for c = (aref (str) i) then (aref (str) i)
                   for denominator = 0 then (if found-decimal (1+ denominator) denominator)
                   if (char= c #\.)
                     do (setq found-decimal t)
                   else
                     collect c into numerator
                   finally
                      (return (* sign
                                 (/ (parse-integer (coerce numerator 'string))
                                    (expt 10 denominator))))))))
      (cond
        ;; Integers
        ((member column-type (list +mysql-type-tiny+
                                   +mysql-type-short+
                                   +mysql-type-long+
                                   +mysql-type-longlong+
                                   +mysql-type-int24+)
                 :test #'=)
         (parse-integer (str)))

        ;; Decimals
        ((member column-type (list +mysql-type-decimal+
                                   +mysql-type-newdecimal+)
                 :test #'=)
         (parse-decimal))

        ;; Floating Point
        ((= column-type +mysql-type-float+)
         (parse-float))
        ((= column-type +mysql-type-double+)
         (parse-float 'double-float))

        ;; Null
        ((= column-type +mysql-type-null+)
         nil)

        ;; Date/Time
        ((member column-type (list +mysql-type-timestamp+
                                   +mysql-type-date+
                                   +mysql-type-time+
                                   +mysql-type-datetime+
                                   +mysql-type-year+
                                   +mysql-type-newdate+)
                 :test #'=)
         (parse-datetime))

        ;; Strings
        ((member column-type (list +mysql-type-varchar+
                                   +mysql-type-var-string+
                                   +mysql-type-string+)
                 :test #'=)
         (str))

        ;; Binary Objects
        ((member column-type (list +mysql-type-bit+
                                   +mysql-type-tiny-blob+
                                   +mysql-type-medium-blob+
                                   +mysql-type-long-blob+
                                   +mysql-type-blob+)
                 :test #'=)
         octets)

        ;; No idea how to parse and represent these yet
        ;; ((= column-type +mysql-type-enum+)
        ;;  octets)
        ;; ((= column-type +mysql-type-set+)
        ;;  octets)
        ;; ((= column-type +mysql-type-geometry+)
        ;;  octets)
        (t
         ;; asedeno-TODO: log unknown type rather than asserting.
         (assert "Unknown type")
         octets)))))
