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

(defclass mysql-prepared-statement ()
  ((connection :type (or mysql-connection null)
               :initarg :connection
               :accessor mysql-prepared-statement-connection)
   (statement-id :type (unsigned-byte 32)
                 :initarg :statement-id
                 :accessor mysql-prepared-statement-statement-id)
   (columns :type (list-of column-definition-v41-packet)
            :initarg :columns
            :accessor mysql-prepared-statement-columns)
   (parameters :type (list-of column-definition-v41)
               :initarg :parameters
               :accessor mysql-prepared-statement-parameters)))
