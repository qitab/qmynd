;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mysqlnd)

;;; MYSQL Authentication Mechanisms

;;; MySQL v5.5.7 added a pluggable authentication mechanism. We don't
;;; support it yet, but there's no reason not to be ready for it.
;;; No plugin is equivalent to MySQL's default: mysql_native_password

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mysql_old_password (15.3.2)

#+mysql-insecure-password-hash
(defun mysql-weak-hash-password (password)
  ;; Here's an implementation of the password hashing function.  The password scramble function used
  ;; for authentication is not implemented.  We don't bother to compile this form by default because
  ;; it's not terribly useful.  http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2000-0981
  (flet ((ub32 (i) (logand #xffffffff i))
         (drop-sign-bit (i) (logand #x7fffffff i)))
    (loop
      for nr = #x50305735 then (ub32 (logxor nr (+ (* (+ (logand nr #x3f) add) c) (ash nr 8))))
      for nr2 = #x12345671 then (ub32 (+ nr2 (logxor (ash nr2 8) nr)))
      for add = 7 then (+ add c)
      for c across password
      finally (return (ironclad:integer-to-octets (logior (ash (drop-sign-bit nr) 32)
                                                          (drop-sign-bit nr2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mysql_native_password (15.3.3)

(defun mysql-native-password-auth-response (password auth-data)
  ;; (xor (sha1 password)
  ;;      (sha1 (concatenate auth-data (sha1 (sha1 password)))))
  (let* ((password-octets (babel:string-to-octets password :encoding (mysql-connection-character-set *mysql-connection*)))
         (hash-stage-1 (ironclad:digest-sequence :sha1 password-octets))
         (hash-stage-2 (ironclad:digest-sequence :sha1 hash-stage-1)))
    (map-into hash-stage-1 #'logxor
              hash-stage-1
              (let ((digester (ironclad:make-digest :sha1)))
                (ironclad:update-digest digester auth-data :end 20)
                (ironclad:update-digest digester hash-stage-2)
                (ironclad:produce-digest digester)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mysql_clear_password (15.3.4)

(defun mysql-clear-password-auth-response (password)
  "This function implements the MySQL clear-text password authentication mechanism."
  (babel:string-to-octets password :encoding (mysql-connection-character-set *mysql-connection*)))

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
    #+mysql-insecure-password-hash
    ((string= plugin "mysql_old_password")
     ;; asedeno-TODO: Replace with an appropriate condition
     (error 'simple-error))
    ((string= plugin "mysql_clear_password")
     (mysql-clear-password-auth-response password))
    ;;; asedeno-TODO: Replace with an appropriate condition
    (T (error 'simple-error))))
