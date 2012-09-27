;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CL-USER")


(asdf:defsystem :cl-mysqlnd-tests
  :name "CL MySQL Native Driver"
  :author "Alejandro Sedeño"
  :version "1.0"
  :licence "MIT-style"
  :maintainer '("Alejandro Sedeño")
  :description      "Test code for MySQL Native Driver for Common Lisp"
  :long-description "Test code for MySQL Native Driver for Common Lisp"
  :defsystem-depends-on (:cl-mysqlnd)
  :depends-on (:babel :flexi-streams :cl-mysqlnd)
  :serial nil
  :components
    ((:module "packages"
              :serial nil
              :pathname #p""
              :components ((:file "pkgdcl")))
     (:module "common"
              :serial nil
              :pathname #p""
              :depends-on ("packages")
              :components ((:file "qtest")))
     (:module "parsing"
              :serial nil
              :pathname #p""
              :depends-on ("common")
              :components ((:file "parsing")
                           (:file "basic-types")))))

(pushnew :cl-mysqlnd *features*)
