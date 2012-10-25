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
           (let* ((payload (mysql-read-packet))
                  (tag (aref payload 0)))
             (cond
               ((or (and (= tag +mysql-response-end-of-file+)
                         (< (length payload) 9))
                    (= tag +mysql-response-error+))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.7.1 Binary Protocol Resultset
;;; Two parts:
;;; • Column Definitions
;;;   · Column Count (n)
;;;   · Column Definition Packets (n total)
;;;   · EOF Packet
;;; • Rows
;;;   · Any number of BinaryResutltsetRow packets
;;;   · EOF Packet

(defun parse-binary-resultset-rows (column-count column-definitions)
  (flet ((parse-binary-resultset-row ()
           (let* ((payload (mysql-read-packet))
                  (tag (aref payload 0)))
             (cond
               ((or (and (= tag +mysql-response-end-of-file+)
                         (< (length payload) 9))
                    (= tag +mysql-response-error+))
                (parse-response payload))
               ((= tag 0)
                (let* ((s (flexi-streams:make-flexi-stream (flexi-streams:make-in-memory-input-stream payload :start 1)))
                       (row (make-array column-count :initial-element nil))
                       (null-bitmap (read-fixed-length-integer (ceiling (+ column-count 2) 8) s)))
                  (loop for i from 0 below column-count
                        when (zerop (ldb (byte 1 (+ i 2)) null-bitmap))
                          do (setf (aref row i)
                                   (parse-binary-protocol-result-column
                                    s
                                    (aref column-definitions i))))
                  row))
               (T (error "unexpected packet parsing binary resultset row"))))))
    (coerce (loop for row = (parse-binary-resultset-row) then (parse-binary-resultset-row)
                  until (typep row 'response-end-of-file-packet)
                  collect row)
                  'vector)))

(defun parse-binary-protocol-result-column (stream column-definition)
  (let ((column-type (column-definition-type column-definition)))
    (labels ((parse-year (&optional (tz 0))
               (encode-universal-time
                0 0 0 1 1
                (read-fixed-length-integer 2 stream)
                tz))
             (parse-decimal (octets)
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
                     (str (babel:octets-to-string octets))
                     found-decimal)
                 (when (= (aref octets 0) #.(char-code #\-))
                   (setq start 1
                         sign -1))
                 (loop
                   for i from start below (length str)
                   for c = (aref str i) then (aref str i)
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
        ;; Stuff encoded as strings
        ((member column-type (list +mysql-type-varchar+
                                   +mysql-type-bit+
                                   +mysql-type-tiny-blob+
                                   +mysql-type-medium-blob+
                                   +mysql-type-blob+
                                   +mysql-type-long-blob+
                                   +mysql-type-var-string+
                                   +mysql-type-string+)
                 :test #'=)
         (read-length-encoded-string stream))

        ((member column-type (list +mysql-type-decimal+
                                   +mysql-type-newdecimal+)
                 :test #'=)
         ;;asedeno-TODO: verify this
         (parse-decimal (read-length-encoded-string stream)))

        ;; Integers
        ((= column-type +mysql-type-longlong+)
         (read-fixed-length-integer 8 stream))

        ((member column-type (list +mysql-type-long+
                                   +mysql-type-int24+) ;; Yes, 24-bit integers are transmitted as 32-bit integers.
                 :test #'=)
         (read-fixed-length-integer 4 stream))

        ((= column-type +mysql-type-short+)
         (read-fixed-length-integer 2 stream))

        ((= column-type +mysql-type-tiny+)
         (read-fixed-length-integer 1 stream))

        ;; Floating Point
        ((= column-type +mysql-type-double+)
         (make-double-float (read-fixed-length-integer 4 stream)
                            (read-fixed-length-integer 4 stream :signed t)))

        ((= column-type +mysql-type-float+)
         (make-single-float (read-fixed-length-integer 4 stream :signed t)))

        ;; Date/Time
        ((= column-type +mysql-type-year+)
         (parse-year))

        ((= column-type +mysql-type-time+)
         (let ((length (read-byte stream))
               (sign 0) (days 0)
               (h 0) (m 0) (s 0) (µs 0))
           ;; NB: We don't support microseconds (µs)
           (assert (member length (list 0 8 12) :test #'=))
           (when (>= length 8)
             (setf sign (if (zerop (read-byte stream)) 1 -1)
                   days (read-fixed-length-integer 4 stream)
                   h (read-byte stream)
                   m (read-byte stream)
                   s (read-byte stream)))
           (when (>= length 12)
             ;; asedeno-TODO: warn about dropped precision
             (setf µs (read-fixed-length-integer 4 stream)))
           (* sign (+ (* days 86400 #|seconds/day|#)
                      (encode-universal-time s m h 1 1 1900 0)))))

        ((member column-type (list +mysql-type-timestamp+
                                   +mysql-type-date+
                                   +mysql-type-datetime+
                                   +mysql-type-newdate+)
                 :test #'=)
         (let ((length (read-byte stream))
               (y 0) (m 1) (d 1)
               (hr 0) (mn 0) (s 0) (µs 0))
           ;; NB: We don't support microseconds (µs)
           (assert (member length (list 0 4 7 11) :test #'=))
           (when (>= length 4)
             (setf y (read-fixed-length-integer 2 stream)
                   m (read-byte stream)
                   d (read-byte stream)))
           (when (>= length 7)
             (setf hr (read-byte stream)
                   mn (read-byte stream)
                   s (read-byte stream)))
           (when (>= length 11)
             ;; asedeno-TODO: warn about dropped precision
             (setf µs (read-fixed-length-integer 4 stream)))
           (encode-universal-time s mn hr d m y 0)))

        ;; These types are not encoded in the binary format:
        ;; +mysql-type-null+ (encoded in null bitmap)
        ;; +mysql-type-enum+
        ;; +mysql-type-set+
        ;; +mysql-type-geometry+
        (t
         ;; asedeno-TODO: signal parsing error
         (assert "Unknown type"))))))
