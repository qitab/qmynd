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
;;; MySQL DateTime

;; MySQL can validate dates in several ways depending on how the server is configured.  Rather than
;; enforcing anything here, we'll allow for the most liberal specification and let the server tell
;; us when we're wrong.

(defclass mysql-date-time ()
  ((year :type (integer 0 #xffff)
         :accessor mysql-date-time-year
         :initarg :year
         :initform 0)
   (month :type (integer 0 12)
          :accessor mysql-date-time-month
          :initarg :month
          :initform 0)
   (day :type (integer 0 31)
        :accessor mysql-date-time-day
        :initarg :day
        :initform 0)
   (hour :type (integer 0 23)
         :accessor mysql-date-time-hour
         :initarg :hour
         :initform 0)
   (minute :type (integer 0 59)
           :accessor mysql-date-time-minute
           :initarg :minute
           :initform 0)
   (second :type (integer 0 59)
           :accessor mysql-date-time-second
           :initarg :second
           :initform 0)
   (microsecond :type (integer 0 999999)
                :accessor mysql-date-time-microsecond
                :initarg :microsecond
                :initform 0)))

(defmethod print-object ((date-time mysql-date-time) stream)
  (with-prefixed-accessors (year month day hour minute second microsecond)
      (mysql-date-time- date-time)
    (format stream "#<MYSQL-DATE-TIME ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D.~6,'0D>"
            year month day hour minute second microsecond)))

(defun mysql-date-time-to-universal-time (date-time)
  "Converts a MySQL DateTime to a Lisp integer-time. Returns NIL if all elements of the date-time
   are zero."
  (assert (typep date-time 'mysql-date-time))
  (with-prefixed-accessors (year month day hour minute second microsecond)
      (mysql-date-time- date-time)
    (unless (every #'zerop (list year month day hour minute second microsecond))
      (assert (>= year 1900))
      ;; asedeno-TODO: log loss of microseconds if non-zero
      (values (encode-universal-time second minute hour day month year 0)
              microsecond))))

(defun universal-time-to-mysql-date-time (integer-time &optional (microseconds 0))
  "Converts a Lisp integer-time to a MySQL DateTime. If integer-time is NIL, returns a MySQL
  DateTime with all elements set to zero."
  (assert (typep integer-time '(or integer null)))
  (if integer-time
      (multiple-value-bind (second minute hour day month year tz)
          (decode-universal-time integer-time 0)
        (declare (ignore tz))
        (make-instance 'mysql-date-time
                       :year year
                       :month month
                       :day day
                       :hour hour
                       :minute minute
                       :second second
                       :microsecond microseconds))
      (make-instance 'mysql-date-time)))

(defun parse-date-time-string (str)
  "Parses a date-time-string in one of the following forms and returns a MYSQL-DATE-TIME object.
    \"\" -- All fields = 0
    \"YYYY-MM-DD\" -- All time fields = 0
    \"YYYY-MM-DD hh:mm:ss\" -- Microseconds = 0
    \"YYYY-MM-DD hh:mm:ss.µµµµµµ\""
  (let ((year 0) (month 0) (day 0)
        (hour 0) (minute 0) (second 0)
        (microsecond 0)
        (length (length str)))
    (when (> length 0)
      ;; YYYY-MM-DD
      (setf year  (parse-integer str :start 0 :end  4)
            month (parse-integer str :start 5 :end  7)
            day   (parse-integer str :start 8 :end 10)))
    (when (> length 10)
      ;; YYYY-MM-DD hh:mm:ss
      (setf hour   (parse-integer str :start 11 :end 13)
            minute (parse-integer str :start 14 :end 16)
            second (parse-integer str :start 17 :end 19)))
    (when (> length 19)
      ;; YYYY-MM-DD hh:mm:ss.µµµµµµ
      (setf microsecond (parse-integer str :start 20 :end 26)))
    (make-instance 'mysql-date-time
                   :year year
                   :month month
                   :day day
                   :hour hour
                   :minute minute
                   :second second
                   :microsecond microsecond)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MySQL Time Interval

(defclass mysql-time-interval ()
  ((negativep :type boolean
              :accessor mysql-time-interval-negativep
              :initarg :negativep
              :initform nil)
   (days :type (integer 0 #xffffffff)
         :accessor mysql-time-interval-days
         :initarg :days
         :initform 0)
   (hours :type (integer 0 23)
          :accessor mysql-time-interval-hours
          :initarg :hours
          :initform 0)
   (minutes :type (integer 0 59)
            :accessor mysql-time-interval-minutes
            :initarg :minutes
            :initform 0)
   (seconds :type (integer 0 59)
            :accessor mysql-time-interval-seconds
            :initarg :seconds
            :initform 0)
   (microseconds :type (integer 0 999999)
                 :accessor mysql-time-interval-microseconds
                 :initarg :microseconds
                 :initform 0)))

(defmethod print-object ((interval mysql-time-interval) stream)
  (with-prefixed-accessors (negativep days hours minutes seconds microseconds)
      (mysql-time-interval- interval)
    (format stream "#<MYSQL-TIME-INTERVAL ~A~D:~2,'0D:~2,'0D.~6,'0D>"
            (if negativep "-" "")
            (+ (* 24 days) hours)
            minutes seconds microseconds)))

(defun mysql-time-interval-to-seconds (interval)
  "Converts a MYSQL-TIME-INTERVAL to a whole number of seconds.
   Returns microseconds as a second value."
  (assert (typep interval 'mysql-time-interval))
  (with-prefixed-accessors (negativep days hours minutes seconds microseconds)
    (mysql-time-interval- interval)
    (values (* (if negativep -1 1)
               (+ seconds
                  (* +seconds-per-minute+
                     (+ minutes
                        (* +minutes-per-hour+
                           (+ hours
                              (* +hours-per-day+ days)))))))
            microseconds)))

(defun seconds-to-mysql-time-interval (value &optional (microseconds 0))
  "Creates a MYSQL-TIME-INTERVAL representing VALUE seconds.
   An optional second argument can be used to specify microseconds."
  (assert (typep value 'integer))
  (let ((negativep (minusp value))
        (value (abs value)))
    (multiple-value-bind (value seconds)   (truncate value +seconds-per-minute+)
      (multiple-value-bind (value minutes) (truncate value +minutes-per-hour+)
        (multiple-value-bind (days hours)  (truncate value +hours-per-day+)
          (make-instance 'mysql-time-interval
                         :negativep negativep
                         :days days
                         :hours hours
                         :minutes minutes
                         :seconds seconds
                         :microseconds microseconds))))))

(defun parse-time-interval-string (str)
  "Parses the MySQL Text Protocol represetation of a time interval.
   /(-)?(h+):(mm):(ss).(µµµµµµ)/"
  (let ((negativep (string-prefix-p "-" str)))
    (multiple-value-bind (hours end)
        (parse-integer str :start (if negativep 1 0) :junk-allowed t)
      (multiple-value-bind (days hours)
          (truncate hours 24)
        (multiple-value-bind (minutes end)
            (parse-integer str :start (1+ end) :junk-allowed t)
          (multiple-value-bind (seconds end)
              (parse-integer str :start (1+ end) :junk-allowed t)
            (let ((microseconds
                    (if (> (length str) end)
                        (parse-integer str :start (1+ end))
                        0)))
              (make-instance 'mysql-time-interval
                             :days days
                             :hours hours
                             :minutes minutes
                             :seconds seconds
                             :microseconds microseconds))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MySQL Year

(defclass mysql-year ()
  ((year :type (integer 0 #xffff)
         :accessor mysql-year-year
         :initarg :year
         :initform 0)))

(defmethod print-object ((year mysql-year) stream)
  (with-prefixed-accessors (year)
      (mysql-year- year)
    (format stream "#<MYSQL-YEAR ~4,'0D>" year)))
