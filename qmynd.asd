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


(asdf:defsystem :qmynd
  :name "MySQL Native Driver"
  :author "Alejandro Sedeño"
  :version "1.0"
  :licence "MIT-style"
  :maintainer '("Alejandro Sedeño")
  :description      "MySQL Native Driver"
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
                     (:file "conditions")
                     (:file "utilities")
                     (:file "date-time"
                      :depends-on ("constants" "utilities"))
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
                     (:file "connection")
                     (:file "response-packets"
                      :depends-on ("define-packet" "connection"))
                     (:file "authentication")
                     (:file "handshake"
                      :depends-on ("define-packet"
                                   "connection"
                                   "authentication"))
                     (:file "response-result-set"
                      :depends-on ("define-packet"))
                     (:module "text-protocol"
                      :serial nil
                      :depends-on ("connection" "define-packet" "response-result-set")
                      :components ((:file "command-quit")
                                   (:file "command-initialize-database")
                                   (:file "command-query")
                                   (:file "command-field-list")
                                   (:file "command-refresh")
                                   (:file "command-shutdown")
                                   (:file "command-statistics")
                                   (:file "command-process-information")
                                   (:file "command-process-kill")
                                   (:file "command-debug")
                                   (:file "command-ping")))
                     (:module "prepared-statements"
                      :serial nil
                      :depends-on ("connection" "response-result-set")
                      :components ((:file "binary-protocol-encoding")
                                   (:file "prepared-statement"
                                    :depends-on ("binary-protocol-encoding"))))))
       (module "api"
        :serial nil
        :depends-on ("mysql-protocol")
        :components ((:file "connection")
                     (:file "prepared-statements")))))))

(pushnew :qmynd *features*)
