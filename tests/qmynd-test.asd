;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :qmynd-test
  :name "MySQL Native Driver - Test Suite"
  :author "Alejandro Sedeño"
  :version "1.0"
  :licence "MIT-style"
  :maintainer '("Alejandro Sedeño")
  :description      "Test code for MySQL Native Driver"
  :long-description "Test code for MySQL Native Driver for Common Lisp"
  :depends-on (:babel :flexi-streams :qmynd)
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
              :components (#-test-tools (:file "qtest")))
     (:module "parsing"
              :serial nil
              :pathname #p""
              :depends-on ("common")
              :components ((:file "parsing")
                           (:file "basic-types")))
     (:module "encoding"
              :serial nil
              :pathname #p""
              :depends-on ("common")
              :components ((:file "binary-encoding")))))
