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

(defun parse-column-count (stream)
  (read-length-encoded-integer stream))

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

(defun column-definition-type (column-definition)
  (column-definition-v41-packet-type column-definition))

(defun column-definition-encoding (column-definition)
  (mysql-cs-coll-to-character-encoding
     (column-definition-v41-packet-cs-coll column-definition)))

(declaim (inline parse-text-protocol-result-column-as-text))

(defun parse-resultset-row (column-count column-definitions
                            &key as-text result-type)
  "Parse a single row of the result set and return either a vector or a
   list, depending on the value of RESULT-TYPE.

   If AS-TEXT is t, return bare data from the server, still dealing with
   encoding of the text columns.

   The default for AS-TEXT is nil, in which case the result columns are
   parsed into native types depending on the meta data passed in
   COLUMNS-DEFINITIONS."
  (let* ((stream (mysql-read-packet))
         (tag    (peek-first-octet stream)))
    (labels
        ((parse-column (str column-definition)
           (when str
             (if as-text
                 (parse-text-protocol-result-column-as-text str column-definition)
                 (parse-text-protocol-result-column str column-definition))))

         (result-as-vector (stream)
           (let ((row (make-array column-count :initial-element nil)))
             (loop for i fixnum from 0 below column-count
                for str = (read-length-encoded-octets stream :null-ok t)
                when str
                do (setf (aref row i)
                         (parse-column str (aref column-definitions i))))
             row))

         (result-as-list (stream)
           (loop for i fixnum from 0 below column-count
              for str = (read-length-encoded-octets stream :null-ok t)
              collect (parse-column str (aref column-definitions i)))))

      (declare (inline parse-column
                       result-as-vector
                       result-as-list))
      (cond
        ((or (and (= tag +mysql-response-end-of-file+)
                  (< (my-len stream) 9))
             (= tag +mysql-response-error+))
         (parse-response stream))
        (t
         (ecase result-type
           (vector  (result-as-vector stream))
           (list    (result-as-list stream))))))))

(defun map-resultset-rows (fn column-count column-definitions
                           &key as-text result-type)
  "Call the FN function with a single row from the result-set at a time.

   When RESULT-TYPE is list, the row is a list, when RESULT-TYPE is vector,
   the row passed to the FN function is a vector."
  (loop for row = (parse-resultset-row column-count
                                       column-definitions
                                       :as-text as-text
                                       :result-type result-type)
     until (typep row 'response-end-of-file-packet)
     do (funcall fn row)))

(defun parse-resultset-rows (column-count column-definitions
                             &key as-text result-type)
  "Accumulate the whole result set in memory then return it as a list or a
   vector depending on the value of RESULT-TYPE (a symbol)."
  (let ((rows
         (loop for row = (parse-resultset-row column-count
                                              column-definitions
                                              :as-text as-text
                                              :result-type result-type)
            until (typep row 'response-end-of-file-packet)
            collect row)))
    (coerce rows result-type)))

(defun decode-octets-to-string (octets &optional encoding)
  "Decode the given vector of OCTETS into an internal Common Lisp string,
  given a known encoding for it. Provide a couple of restarts in case the
  decoding fails:

    - use-nil             decode octets as a nil value
    - use-empty-string    decode octets as an empty string (\"\")
    - use-value           decode octets as any given value."
  (let ((encoding (or *mysql-encoding*
                      encoding
                      babel::*default-character-encoding*)))
    (restart-case (babel:octets-to-string octets :encoding encoding)
      (use-nil ()
        :report "skip this column's value and use nil instead."
        nil)
      (use-empty-string ()
        :report "skip this column's value and use and empty-string instead."
        "")
      (use-value (value) value))))

(defun parse-text-protocol-result-column-as-text (octets column-definition)
  "Refrain from parsing data into lisp types, some application will only use
   the text form anyway"
  (let ((column-type (column-definition-type column-definition)))
    (cond ((= column-type +mysql-type-null+)
           nil)

          ;; support for BLOB and TEXT types
          ((member column-type (list +mysql-type-tiny-blob+
                                     +mysql-type-medium-blob+
                                     +mysql-type-long-blob+
                                     +mysql-type-blob+))
           (if (= (column-definition-v41-packet-cs-coll column-definition)
                  +mysql-cs-coll-binary+)
               octets
               (let ((encoding (column-definition-encoding column-definition)))
                 (decode-octets-to-string octets encoding))))

          ;; Binary types
          ((member column-type (list +mysql-type-bit+
                                     +mysql-type-enum+
                                     +mysql-type-set+
                                     +mysql-type-geometry+)
                   :test #'=)
           octets)

          ;; binary strings are strings with binary collations...
          ((and (member column-type (list +mysql-type-string+
                                          +mysql-type-var-string+))
                (= (column-definition-v41-packet-cs-coll column-definition)
                   +mysql-cs-coll-binary+))
           octets)

          (t
           (let ((encoding (column-definition-encoding column-definition)))
             (decode-octets-to-string octets encoding))))))

(defun parse-text-protocol-result-column (octets column-definition)
  (let ((column-type (column-definition-type column-definition))
        str)
    (labels ((str ()
               (unless str (setf str (decode-octets-to-string octets)))
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
           (let* ((my-stream (mysql-read-packet))
                  (tag (peek-first-octet my-stream)))

             (cond
               ((or (and (= tag +mysql-response-end-of-file+)
                         (< (my-len my-stream) 9))
                    (= tag +mysql-response-error+))
                (parse-response my-stream))
               ((= tag 0)
                (let ((s my-stream))
                  (read-fixed-length-octets 1 s)
                  (let* ((row (make-array column-count :initial-element nil))
                         (null-bitmap (read-fixed-length-integer (ceiling (+ column-count 2) 8) s)))
                    (loop for i from 0 below column-count
                          when (zerop (ldb (byte 1 (+ i 2)) null-bitmap))
                            do (setf (aref row i)
                                     (parse-binary-protocol-result-column
                                      s
                                      (aref column-definitions i))))
                    row)))
               (t (error (make-condition 'unexpected-packet :payload (my-payload my-stream))))))))
    (coerce (loop for row = (parse-binary-resultset-row) then (parse-binary-resultset-row)
                  until (typep row 'response-end-of-file-packet)
                  collect row)
                  'vector)))

(defun parse-binary-protocol-result-column (stream column-definition)
  (let ((column-type (column-definition-type column-definition))
        (encoding (column-definition-encoding column-definition)))
    (labels ((to-string (octets)
               (if encoding
                   (decode-octets-to-string octets encoding)
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
         (let ((octets (read-length-encoded-octets stream)))
           (to-string octets)))

        ((member column-type (list +mysql-type-decimal+
                                   +mysql-type-newdecimal+)
                 :test #'=)
         (parse-decimal (to-string (read-length-encoded-octets stream))))

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
         (read-rest-of-packet-octets stream))
        ;; +mysql-type-null+ (encoded in null bitmap)
        (t
         ;; asedeno-TODO: log unknown type
         (read-rest-of-packet-octets stream))))))
