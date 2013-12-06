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

;;; The parameter "column definition" does not specify types well enough to be used for encoding
;;; here. Instead, we'll pick an encoding based on the type of the parameter. Later we may allow the
;;; caller to override that type.
(defun encode-binary-parameter (value value-stream type-stream)
  (flet ((encode-binary-integer (length)
           "Writes an n-octet integer to VALUE-STREAM and the type flags (signed vs unsigned) to
            TYPE-STREAM, assumes a type code has already been written to TYPE-STREAM."
           (assert (typep value 'integer))
           (write-fixed-length-integer value length value-stream)
           (write-byte (if (minusp value) #x00 #x80) type-stream)))

    (etypecase value
      ;; Octets
      ((vector (unsigned-byte 8))
       (write-length-encoded-octets value value-stream)
       (write-byte +mysql-type-var-string+ type-stream)
       (write-byte #x00 type-stream))

      ;; Strings (→ octets)
      (string
       (encode-binary-parameter (babel:string-to-octets value) value-stream type-stream))

      ;; Integers
      ;; tiny, signed or unsigned
      ((or bit (integer #x-80 #xff))
       (write-byte +mysql-type-tiny+ type-stream)
       (encode-binary-integer 1))
      ;; short, signed or unsigned
      ((integer #x-8000 #xffff)
       (write-byte +mysql-type-short+ type-stream)
       (encode-binary-integer 2))
      ;; long, signed or unsigned
      ((integer #x-80000000 #xffffffff)
       (write-byte +mysql-type-long+ type-stream)
       (encode-binary-integer 4))
      ;; long-long, signed or unsigned
      ((integer #x-8000000000000000 #xffffffffffffffff)
       (write-byte +mysql-type-longlong+ type-stream)
       (encode-binary-integer 8))
      ;; too big, encode as string and let MySQL deal with it.
      (integer
       (encode-binary-parameter (princ-to-string value) value-stream type-stream))

      ;; Ratios (→ Decimal String if possible, else Double)
      (ratio
       (handler-case
           (encode-binary-parameter (write-decimal-to-string value) value-stream type-stream)
         (value-is-not-decimal ()
           (encode-binary-parameter (coerce value 'double-float) value-stream type-stream))))

      ;; Floating point
      (single-float
       (write-fixed-length-integer (single-float-bits value)
                                   4 value-stream)
       (write-byte +mysql-type-float+ type-stream)
       (write-byte #x80 type-stream))
      (double-float
       (multiple-value-bind (l h) (double-float-bits value)
         (write-fixed-length-integer l 4 value-stream)
         (write-fixed-length-integer h 4 value-stream))
       (write-byte +mysql-type-double+ type-stream)
       (write-byte #x80 type-stream))

      ;; MySQL Date/Time structs
      (mysql-date-time
       (with-prefixed-accessors (year month day hour minute second microsecond)
           (mysql-date-time- value)
         (let ((length (cond
                         ((= 0 year month day hour minute second microsecond) 0)
                         ((= 0 hour minute second microsecond) 4)
                         ((= 0 microsecond) 7)
                         (t 11))))
           (write-byte length value-stream)
           (when (> length 0)
             (write-fixed-length-integer year 2 value-stream)
             (write-byte month value-stream)
             (write-byte day value-stream))
           (when (> length 4)
             (write-byte hour value-stream)
             (write-byte minute value-stream)
             (write-byte second value-stream))
           (when (> length 7)
             (write-fixed-length-integer microsecond 4 value-stream))))
       (write-byte +mysql-type-datetime+ type-stream)
       (write-byte 0 type-stream))

      (mysql-time-interval
       (with-prefixed-accessors (negativep days hours minutes seconds microseconds)
           (mysql-time-interval- value)
         (let ((length (cond
                         ((= 0 days hours minutes seconds microseconds) 0)
                         ((= 0 microseconds) 8)
                         (t 12))))
           (write-byte length value-stream)
           (when (> length 0)
             (write-byte (if negativep 1 0) value-stream)
             (write-fixed-length-integer days 4 value-stream)
             (write-byte hours value-stream)
             (write-byte minutes value-stream)
             (write-byte seconds value-stream))
           (when (> length 8)
             (write-fixed-length-integer microseconds 4 value-stream))))
       (write-byte +mysql-type-time+ type-stream)
       (write-byte 0 type-stream))

      (mysql-year
       (write-fixed-length-integer (mysql-year-year value) 2 value-stream)
       (write-byte +mysql-type-year+ type-stream)
       (write-byte 0 type-stream)))))
