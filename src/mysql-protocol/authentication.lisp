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

;;; MYSQL Authentication Mechanisms

;;; MySQL v5.5.7 added a pluggable authentication mechanism. We don't
;;; support it yet, but there's no reason not to be ready for it.
;;; No plugin is equivalent to MySQL's default: mysql_native_password

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mysql_old_password (15.3.2)

(defun mysql-weak-hash-password (password)
  ;; having that allows to support mysql servers without
  ;; +mysql-capability-client-long-password+
  (if (null password)
      (make-array 8 :element-type '(unsigned-byte 8) :initial-element 32)
      (flet ((ub32 (i)          (logand #xffffffff i))
             (drop-sign-bit (i) (logand #x7fffffff i)))
        (let ((nr  #x50305735)
              (nr2 #x12345671)
              (add 7))
          (loop
             for c across (typecase password
                            (string
                             (babel:string-to-octets password :encoding :ascii))
                            (t password))
             do (progn
                  (setf nr  (ub32 (logxor nr (+ (* (+ (logand nr #x3f) add) c)
                                                (ash nr 8))))
                        nr2 (ub32 (+ nr2 (logxor (ash nr2 8) nr)))
                        add (+ add c))))
          (ironclad:integer-to-octets
           (logior (ash (drop-sign-bit nr) 32)
                   (drop-sign-bit nr2)))))))

(defstruct mysql-rand-st max-value max-value-dbl seed1 seed2)

(defun mysql-old-random-init (seed1 seed2)
  (let ((max-value #x3FFFFFFF))
   (make-mysql-rand-st :max-value max-value
                       :max-value-dbl (* 1.0d0 max-value)
                       :seed1 (mod seed1 max-value)
                       :seed2 (mod seed2 max-value))))

(defun my-rnd (rand-st)
  (setf (mysql-rand-st-seed1 rand-st)
        (mod (+ (* 3 (mysql-rand-st-seed1 rand-st)) (mysql-rand-st-seed2 rand-st))
             (mysql-rand-st-max-value rand-st))

        (mysql-rand-st-seed2 rand-st)
        (mod (+ 33 (mysql-rand-st-seed1 rand-st) (mysql-rand-st-seed2 rand-st))
             (mysql-rand-st-max-value rand-st)))

  (/ (mysql-rand-st-seed1 rand-st) (mysql-rand-st-max-value-dbl rand-st)))

(defun mysql-old-password-auth-response (password auth-data)
  "Scramble password hash with first 8 bytes of auth-data."
  (let* ((password-hash (mysql-weak-hash-password password))
         (message-hash  (mysql-weak-hash-password (subseq auth-data 0 8)))
         (rand-st
          (mysql-old-random-init
           (logxor (ironclad:octets-to-integer password-hash :end 4)
                   (ironclad:octets-to-integer message-hash :end 4))
           (logxor (ironclad:octets-to-integer password-hash :start 4)
                   (ironclad:octets-to-integer message-hash :start 4))))
         (scrambled (make-array 8 :element-type '(unsigned-byte 8))))

    (loop for i below 8
       do (setf (aref scrambled i)
                (+ 64 (floor (* 31 (my-rnd rand-st))))))

    (let ((extra (floor (* 31 (my-rnd rand-st)))))
      (loop for i below 8
         do (setf (aref scrambled i)
                  (logxor (aref scrambled i) extra))))

    scrambled))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mysql_native_password (15.3.3)

(defun mysql-native-password-auth-response (password auth-data)
  ;; (xor (sha1 password)
  ;;      (sha1 (concatenate auth-data (sha1 (sha1 password)))))
  (if (null password)
      ""
      (let* ((password-octets (babel:string-to-octets password))
             (hash-stage-1 (ironclad:digest-sequence :sha1 password-octets))
             (hash-stage-2 (ironclad:digest-sequence :sha1 hash-stage-1)))
        (map-into hash-stage-1 #'logxor
                  hash-stage-1
                  (let ((digester (ironclad:make-digest :sha1)))
                    (ironclad:update-digest digester auth-data :end 20)
                    (ironclad:update-digest digester hash-stage-2)
                    (ironclad:produce-digest digester))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mysql_clear_password (15.3.4)

(defun mysql-clear-password-auth-response (password)
  "This function implements the MySQL clear-text password authentication mechanism."
  (babel:string-to-octets password))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authentication_windows_client (15.3.5)
;;; Not implemented

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sha256_password (15.3.6)
;;; Not implemented

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Authentication mechanism dispatcher

(defun generate-auth-response (password auth-data &optional plugin)
  (cond
    ((or (not plugin)
         (string= plugin "mysql_native_password"))
     (mysql-native-password-auth-response password auth-data))
    ((string= plugin "mysql_old_password")
     (mysql-old-password-auth-response password auth-data))
    ((string= plugin "mysql_clear_password")
     (mysql-clear-password-auth-response password))
    (T
     (error (make-condition 'mysql-unsupported-authentication
                            :plugin plugin)))))
