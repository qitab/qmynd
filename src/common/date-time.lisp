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
;;; MySQL DateTime

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

;; MySQL can validate dates in several ways depending on how the server is configured.  Rather than
;; enforcing anything here, we'll allow for the most liberal specification and let the server tell
;; us when we're wrong.

;; (defmethod initialize-instance :after ((date-time mysql-date-time) &key)
;;   (with-prefixed-accessors (year month day hour minute second microsecond)
;;       (mysql-date-time- date-time)
;;     (unless (= 0 year month day hour minute second microsecond)
;;       (when (or (zerop month)
;;                 (zerop month)
;;                 (and (member month '(2 4 6 9 11))
;;                      (> day 30))
;;                 (and (= month 2)
;;                      (cond
;;                        ((zerop (mod year 400))
;;                         (> day 29))
;;                        ((zerop (mod year 100))
;;                         (> day 28))
;;                        ((zerop (mod year 4))
;;                         (> day 29))
;;                        (t (> day 28)))))
;;         (error 'invalid-date-time-parameter :value date-time)))))

(defun mysql-date-time-to-universal-time (date-time)
  (assert (typep date-time 'mysql-date-time))
  (with-prefixed-accessors (year month day hour minute second microsecond)
      (mysql-date-time- date-time)
    (assert (>= year 1900))
    ;; asedeno-TODO: warn on loss of microseconds
    (encode-universal-time second minute hour day month year 0)))

(defun universal-time-to-mysql-date-time (integer-time)
  (assert (typep integer-time 'integer))
  (multiple-value-bind (second minute hour day month year tz)
      (decode-universal-time integer-time 0)
    (declare (ignore tz))
    (make-instance 'mysql-date-time
                   :year year
                   :month month
                   :day day
                   :hour hour
                   :minute minute
                   :second second)))

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
  (assert (typep interval 'mysql-time-interval))
  (with-prefixed-accessors (negativep days hours minutes seconds microseconds)
      (mysql-time-interval- interval)
    (* (if negativep -1 1)
       (+ seconds
          (* +seconds-per-minute+
             (+ minutes
                (* +minutes-per-hour+
                   (+ hours
                      (* +hours-per-day+ days)))))))))

(defun seconds-to-mysql-time-interval (value)
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
                         :seconds seconds))))))

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

