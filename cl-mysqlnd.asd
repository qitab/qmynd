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


(asdf:defsystem :cl-mysqlnd
  :name "CL MySQL Native Driver"
  :author "Alejandro Sedeño"
  :version "1.0"
  :licence "MIT-style"
  :maintainer '("Alejandro Sedeño")
  :description      "MySQL Native Driver for Common Lisp"
  :long-description "MySQL Native Driver for Common Lisp"
  :depends-on (:babel :flexi-streams :ironclad :usocket)
  :serial nil
  :components
    ((:module "src"
      :serial nil
      :components
      ((:file "pkgdcl")
       (:module "common"
        :serial nil
        :depends-on ("pkgdcl")
        :components ((:file "constants")
                     (:file "utilities")
                     (:file "misc"
                      :depends-on ("constants"))))
       (:module "wire-protocol"
        :serial nil
        :depends-on ("common")
        :components ((:file "basic-types")
                     (:file "wire-packet"
                      :depends-on ("basic-types"))))
       (module "mysql-protocol"
        :serial nil
        :depends-on ("common" "wire-protocol")
        :components ((:file "define-packet")
                     (:file "response-packets"
                      :depends-on ("define-packet"))
                     (:file "connection")
                     (:file "authentication")
                     (:file "handshake"
                      :depends-on ("define-packet"
                                   "connection"
                                   "authentication"))))))))

(pushnew :cl-mysqlnd *features*)
