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

(defgeneric column-definition-type (column-definition)
  (:method ((column-definition column-definition-v41-packet))
    (column-definition-v41-packet-type column-definition)))

(defgeneric column-definition-encoding (column-definition)
  (:method ((column-definition column-definition-v41-packet))
    (mysql-cs-coll-to-character-encoding
     (column-definition-v41-packet-cs-coll column-definition))))

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
                        for str = (read-length-encoded-string s :null-ok t)
                        when str
                          do (setf (aref row i)
                                   (parse-text-protocol-result-column
                                    str
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
               (unless str (setf str
                                 (babel:octets-to-string
                                  octets)))
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
                     (read s))))))
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
         (parse-decimal (str)))

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
                                   +mysql-type-datetime+
                                   +mysql-type-newdate+)
                 :test #'=)
         (parse-date-time-string (str)))

        ((= column-type +mysql-type-year+)
         (make-instance 'mysql-year
                        :year (parse-integer (str))))

        ((= column-type +mysql-type-time+)
         (parse-time-interval-string (str)))

        ;; Strings and Binary Objects
        ((member column-type (list +mysql-type-varchar+
                                   +mysql-type-var-string+
                                   +mysql-type-string+
                                   +mysql-type-bit+
                                   +mysql-type-tiny-blob+
                                   +mysql-type-medium-blob+
                                   +mysql-type-long-blob+
                                   +mysql-type-blob+)
                 :test #'=)
         (let ((encoding (column-definition-encoding column-definition)))
           (cond (encoding
                  (let ((babel::*default-character-encoding* encoding))
                    (str)))
                 (t octets))))

        ;; No idea how to parse and represent these yet
        ;; ((= column-type +mysql-type-enum+)
        ;;  octets)
        ;; ((= column-type +mysql-type-set+)
        ;;  octets)
        ;; ((= column-type +mysql-type-geometry+)
        ;;  octets)
        (t
         ;; asedeno-TODO: log unknown type
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
                (let* ((s (flexi-streams:make-in-memory-input-stream payload :start 1))
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
  (let ((column-type (column-definition-type column-definition))
        (encoding (column-definition-encoding column-definition)))
    (labels ((to-string (octets)
               (if encoding
                 (babel:octets-to-string octets :encoding encoding)
                 octets))
             (parse-binary-integer (length)
               (read-fixed-length-integer
                length stream
                :signed (not (flagsp +mysql-flag-column-unsigned+
                                     (column-definition-v41-packet-flags column-definition))))))
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
         (let ((octets (read-length-encoded-string stream)))
           (to-string octets)))

        ((member column-type (list +mysql-type-decimal+
                                   +mysql-type-newdecimal+)
                 :test #'=)
         (parse-decimal (to-string (read-length-encoded-string stream))))

        ;; Integers
        ((= column-type +mysql-type-longlong+)
         (parse-binary-integer 8))

        ((member column-type (list +mysql-type-long+
                                   +mysql-type-int24+) ;; Yes, 24-bit integers are transmitted as 32-bit integers.
                 :test #'=)
         (parse-binary-integer 4))

        ((= column-type +mysql-type-short+)
         (parse-binary-integer 2))

        ((= column-type +mysql-type-tiny+)
         (parse-binary-integer 1))

        ;; Floating Point
        ((= column-type +mysql-type-double+)
         (make-double-float (read-fixed-length-integer 4 stream)
                            (read-fixed-length-integer 4 stream :signed t)))

        ((= column-type +mysql-type-float+)
         (make-single-float (read-fixed-length-integer 4 stream :signed t)))

        ;; Date/Time
        ((= column-type +mysql-type-year+)
         (let ((year (read-fixed-length-integer 2 stream)))
           (make-instance 'mysql-year :year year)))

        ((= column-type +mysql-type-time+)
         (let ((length (read-byte stream))
               negativep
               (days 0) (h 0) (m 0) (s 0) (µs 0))
           (assert (member length (list 0 8 12) :test #'=))
           (when (>= length 8)
             (setf negativep (not (zerop (read-byte stream)))
                   days (read-fixed-length-integer 4 stream)
                   h (read-byte stream)
                   m (read-byte stream)
                   s (read-byte stream)))
           (when (>= length 12)
             (setf µs (read-fixed-length-integer 4 stream)))
           (make-instance 'mysql-time-interval
                          :negativep negativep
                          :days days
                          :hours h
                          :minutes m
                          :seconds s
                          :microseconds µs)))

        ((member column-type (list +mysql-type-timestamp+
                                   +mysql-type-date+
                                   +mysql-type-datetime+
                                   +mysql-type-newdate+)
                 :test #'=)
         (let ((length (read-byte stream))
               (y 0) (m 0) (d 0)
               (hr 0) (mn 0) (s 0) (µs 0))
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
             (setf µs (read-fixed-length-integer 4 stream)))
           (make-instance 'mysql-date-time
                          :year y
                          :month m
                          :day d
                          :hour hr
                          :minute mn
                          :second s
                          :microsecond µs)))

        ((member column-type (list +mysql-type-enum+
                                   +mysql-type-set+
                                   +mysql-type-geometry+))
         (read-rest-of-packet-string stream))
        ;; +mysql-type-null+ (encoded in null bitmap)
        (t
         ;; asedeno-TODO: log unknown type
         (read-rest-of-packet-string stream))))))
