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
;;; 15.6.4 command-query

;; We don't actually receive this packet as a client, but it looks like this.

;; (define-packet command-query
;;   ((tag :mysql-type (integer 1) :value +mysql-command-query+ :transient t :bind nil)
;;    (query-string :mysql-type (string :eof))))

(defun send-command-query (query-string
			   &key
			     row-fn
			     (as-text nil)
			     (result-type 'vector))
  "Send QUERY-STRING to the current MySQL connection.

   When the ROW-FN parameter is given, it must be a function and is called
   with each row as input, and the rows are discarded once the function is
   called.

   When AS-TEXT is t, the column values are not converted to native types
   and returned as text instead.

   By default the resultset is a vector of rows where each row is itself a
   vector of columns. When RESULT-TYPE is list, the result is a list of list
   of columns instead."
  (mysql-command-init +mysql-command-query+)
  (mysql-write-packet
   (flexi-streams:with-output-to-sequence (s)
     (write-byte +mysql-command-query+ s)
     (write-sequence (babel:string-to-octets query-string) s)))
  (let* ((my-stream (mysql-read-packet))
         (tag       (peek-first-octet my-stream)))
    (if (member tag (list +mysql-response-ok+ +mysql-response-error+))
        (parse-response my-stream)
        (let* ((column-count (parse-column-count my-stream))
               (column-definitions
		(coerce
		 (loop
		    repeat column-count
		    collect (parse-column-definition-v41 (mysql-read-packet))
		    ;; Consume the EOF packet or signal an error for an ERR packet.
		    finally (parse-response (mysql-read-packet)))
		 'vector))
               (rows
		(if row-fn
		    (map-resultset-rows row-fn column-count column-definitions
					:as-text as-text
					:result-type result-type)
		    (parse-resultset-rows column-count column-definitions
					  :as-text as-text
					  :result-type result-type))))
          (values rows column-definitions)))))
