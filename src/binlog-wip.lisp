;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2017      TurtleWare    All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Daniel Kochmański                               ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.9.9 command-register-slave

;; We don't actually receive this packet as a client, but it looks like this.

#+ (or)
(define-packet command-register-slave
    ;;; In return we expect `OK' or `ERR' packet.
    ((tag :mysql-type (integer 1) :value +com-register-slave+ :transient t :bind nil)
     (server-id         :mysql-type (integer 4) :value +slave-id+)
     (slaves-host-len   :mysql-type (integer 1))
     (slaves-hostname   :mysql-type (string :lenenc))
     (slaves-user-len   :mysql-type (integer 1))
     (slaves-user       :mysql-type (string :lenenc))
     (slaves-pass-len   :mysql-type (integer 1))
     (slaves-pass       :mysql-type (string :lenenc))
     (slaves-mysql-port :mysql-type (integer 2))
     (replication-rank  :mysql-type (octets 4))
     (master-id         :mysql-type (integer 4))))

(defun send-command-register-slave (slave-id)
  (mysql-command-init +mysql-command-register-slave+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-fixed-length-integer +mysql-command-register-slave+ 1 s)
     (write-fixed-length-integer slave-id                       4 s)
     #| next 5 fields are usually ignored |#
     (write-fixed-length-integer 0 1 s) ; slave hostname
     (write-fixed-length-integer 0 1 s) ; username
     (write-fixed-length-integer 0 1 s) ; password
     (write-fixed-length-integer 0 2 s) ; slaves-port
     ;; (write-length-encoded-octets (babel:string-to-octets "localhost") s)
     ;; (write-length-encoded-octets (babel:string-to-octets username)    s)
     ;; (write-length-encoded-octets (babel:string-to-octets password)    s)
     ;; (write-fixed-length-integer  +slave-port+ 2 s)
     #| replication rank is not used |#
     (write-fixed-length-integer 0 4 s)
     ;; master ID, 0 is OK
     (write-fixed-length-integer 0 4 s)))
  (parse-response (mysql-read-packet)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.9.5 command-binary-log-dump

;; We don't actually receive this packet as a client, but it looks like this.
#+ (or)
(define-packet command-binary-log-dump
    ;;; In return we expect `binary-log-event' stream or `EOF' packet.
    ((tag :mysql-type (integer 1) :value +mysql-command-binary-log-dump+ :transient t :bind nil)
     (binlog-pos      :mysql-type (integer 4))
     (flags           :mysql-type (integer 2)) ; `+mysql-flag-binary-log-dump-non-block+'
     (server-id       :mysql-type (integer 4) :value +slave-id+)
     (binlog-filename :mysql-type (string :eof))))

(defun send-command-binary-log-dump (slave-id binary-log-position
                                     &optional non-blocking binary-log-filename)
  (mysql-command-init +mysql-command-binary-log-dump+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-fixed-length-integer +mysql-command-binary-log-dump+ 1 s)
     (write-fixed-length-integer binary-log-position             4 s)
     (write-fixed-length-integer
      (if (null non-blocking)
          #x00
          +mysql-flag-binary-log-dump-non-block+)
      2 s)
     (write-fixed-length-integer slave-id 4 s) ; slave server id (unique)
     ;; binlog filename
     (when binary-log-filename
       (write-sequence (babel:string-to-octets binary-log-filename) s)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.9.6 command-binary-log-dump-gtid
;;; http://imysql.com/mysql-internal-manual/com-binlog-dump-gtid.html

;; We don't actually receive this packet as a client, but it looks like this.
#+ (or)
(define-packet command-binary-log-dump-gtid
    ;;; In return we expect `binary-log-event' stream or `EOF' packet.
    ((tag :mysql-type (integer 1) :value +mysql-command-binary-log-dump+ :transient t :bind nil)
     (flags               :mysql-type (integer 2))
     (server-id           :mysql-type (integer 4) :value +slave-id+)
     (binlog-filename-len :mysql-type (integer 4))
     (binlog-filename     :mysql-type (string :lenenc))
     (binlog-pos          :mysql-type (integer 4))))

(defun send-command-binary-log-dump-gtid (slave-id binary-log-position
                                          &optional flags binary-log-filename)
  (mysql-command-init +mysql-command-binary-log-dump+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-fixed-length-integer +mysql-command-binary-log-dump+ 1 s)
     (write-fixed-length-integer flags 2 s)
     (write-fixed-length-integer slave-id 4 s) ; slave server id (unique)
     (write-length-encoded-octets (babel:string-to-octets binary-log-filename) s)
     (write-fixed-length-integer binary-log-position 4 s)

     #+ (or)                            ; gtid-data is SID block
     (when (logand flags +mysql-flag-binary-log-dump-through-gtid+)
       (let ((octets (babel:string-to-octets gtid-data)))
         (write-fixed-length-integer (length octets) 4 s)
         (write-sequence octets s))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.9.7 command-table-dump

;; We don't actually receive this packet as a client, but it looks like this.
#+ (or)
(define-packet command-table-dump
    ;;; In return we expect a table dump or `ERR' packet.
    ((tag :mysql-type (integer 1) :value +mysql-command-table-dump+ :transient t :bind nil)
     (database-length  :mysql-type (integer 1))
     (database-name    :mysql-type (string :lenenc))
     (table-length     :mysql-type (integer 1))
     (table-name       :mysql-type (string :lenenc))))

(defun send-command-table-dump (database table)
  (mysql-command-init +mysql-command-table-dump+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-fixed-length-integer +mysql-command-table-dump+ 1 s)
     (write-length-encoded-octets (babel:string-to-octets database) s)
     (write-length-encoded-octets (babel:string-to-octets table)    s))))



(define-packet common-header
    ((ok-tag          :mysql-type (integer 1) :bind nil :transient t)
     (when-started    :mysql-type (integer 4))
     (type-code       :mysql-type (integer 1))
     (master-id       :mysql-type (integer 4))
     (data-written    :mysql-type (integer 4))
     (binlog-position :mysql-type (integer 4))
     (flags           :mysql-type (integer 2))))

(define-packet rotate-event
    ((position :mysql-type (integer 8))
     (new-log-ident :mysql-type (string :eof))))

(define-packet query-event
    ((thread-id  :mysql-type (integer 4))
     (exec-time  :mysql-type (integer 4))
     ;; The length of the name of the currently selected
     ;; database. Ignored, because name is null-terminated.
     (selected-db-length :mysql-type (integer 1) :bind nil :transient t)
     ;; Error code generated by the master. If the master fails, the
     ;; slave will fail with the same error code.
     (error-code         :mysql-type (integer 2))
     (status-vars-len    :mysql-type (integer 2) :transient t)
     ;; "body"
     (status-vars        :mysql-type (octets status-vars-len))
     (selected-db        :mysql-type (string :null))
     (query-string       :mysql-type (string :eof))))

(define-packet table-event
    ((table-id :mysql-type (integer 6))
     ;; reserved for future use
     (flags         :mysql-type (integer 2) :bind nil :transient t)
     (database-name :mysql-type (string :lenenc-null-ok))
     (table-name    :mysql-type (string :lenenc-null-ok))
     (column-count  :mysql-type (integer :lenenc) :transient t)
     (column-type   :mysql-type (octets column-count))
     (metadata-length :mysql-type (integer :lenenc) :transient t)
     (metadata  :mysql-type (octets metadata-length))
     (null-bits :mysql-type (octets (ceiling column-count 8)))
     (optional-meta :mysql-type (octets :eof))))

(defun parse-binlog-event (packet)
  (when (/= (peek-first-octet packet) +mysql-response-ok+)
    (return-from parse-binlog-event (parse-response packet)))

  (let ((common-header (parse-common-header packet)))

    (alexandria:switch ((common-header-packet-type-code common-header))

      (+table-map-event+ (parse-table-event packet))

      ;; https://dev.mysql.com/doc/dev/mysql-server/latest/classbinary__log_1_1Format__description__event.html
      (+format-description-event+ "(Unimplemented) Format Description")

      (+rotate-event+ (parse-rotate-event packet))
      (+query-event+ (parse-query-event packet))

      (+write-rows-event-v1+ "Write rows (V1)")
      (+update-rows-event-v1+ "Update rows (V1)")
      (+delete-rows-event-v1+ "Delete rows (V1)")

      (+gtid-log-event+ "GTID")

      (+xid-event+ "XID")
      (+heartbeat-log-event+ "HeartBeat")
      (+intvar-event+  "IntVar")

      ;;; no-op

      ;; It is the minimal constructor, and all it will do is set the
      ;; type_code as `+stop-event' in the common-header in
      ;; `binlog-event'.
      (+stop-event+    "Stop")

      ;; This constructor will initialize the instance variables and
      ;; the `type-code', it will be used only by the server code.
      ;;
      ;; XXX: I'm not sure what that means. Can we ignore this event? -- jd
      ;; https://dev.mysql.com/doc/dev/mysql-server/latest/classbinary__log_1_1User__var__event.html
      (+user-var-event+ (format nil "User Var: ~s"
                                (qmynd-impl::read-rest-of-packet-octets packet)))

      ;; Self-explanatory
      (+unknown-event+ "Unknown")

      ;;; unimplemented
      (+write-rows-event-v2+  "(Unimplemented) Write rows (V2)")
      (+update-rows-event-v2+ "(Unimplemented) Update rows (V2)")
      (+delete-rows-event-v2+ "(Unimplemented) Delete rows (V2)")
      (otherwise (format nil "Unimplemented 0x~2,'0x"
                         (common-header-packet-type-code common-header))))))

(defmethod print-object ((object common-header-packet) s)
  (format s "#<Event type 0x~2,'0x position: ~s>"
          (common-header-packet-type-code object)
          (common-header-packet-binlog-position object)))

;;;; Types

;;; errors
(define-condition binlog-error (mysql-base-error) ())
(define-condition event-error (binlog-error) ())

;;; connection
(defclass binlog-connection (mysql-local-connection) ())

;;; events
(defclass binlog-event () ())
(defclass binlog-rows-event (binlog-event) ())
(defclass binlog-not-implemented-event (binlog-event) ())
(defclass binlog-gtid-event (binlog-event) ())
(defclass binlog-rotate-event (binlog-event) ())
(defclass binlog-format-description-event (binlog-event) ())
(defclass binlog-stop-event (binlog-event) ())
(defclass binlog-xid-event (binlog-event) ())
(defclass binlog-heartbeat-event (binlog-event) ())
(defclass binlog-query-event (binlog-event) ())
(defclass binlog-begin-load-query-event (binlog-event) ())
(defclass binlog-execute-load-query-event (binlog-event) ())
(defclass binlog-intvar-event (binlog-event) ())
(defclass binlog-table-map-event (binlog-event) ())
(defclass binlog-delete-rows-event (binlog-rows-event) ())
(defclass binlog-write-rows-event (binlog-rows-event) ())
(defclass binlog-update-rows-event (binlog-rows-event) ())


(defun slurp-packets (connection &optional (file "") (position 4))
  (when (< position 4)
    (log:warn "correcting invalid binglog postion from ~s to 4." position)
    (setf position 4))
  (qmynd-impl:with-mysql-connection (connection)
    (qmynd-impl::send-command-register-slave 7)
    (qmynd-impl::send-command-binary-log-dump 7 position t file)
    (do ((c 0 (1+ c))
         (r #1=(qmynd-impl::parse-binlog-event (qmynd-impl:mysql-read-packet))
            (progn
              (format t "Read ~3s packet: ~s~%" c r)
              #1#)))
        ((typep r 'qmynd-impl::response-end-of-file-packet)
         (format t "Read ~A packets!~%" c)))))
