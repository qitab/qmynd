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
;;; Packet Definition Macrology

#||
Specs:

slot-specifier ::= (slot-name [[slot-options]])
slot-name ::= symbol
slot-options ::= {:bind boolean } |
                 {:eof eof-action} |
                 {:mysql-type parser-type-spec} |
                 {:predicate form} |
                 {:reduce λ α′ α → β)
                 {:transform λ α → β} |
                 {:transient boolean} |
                 {:type type-specifier} |
                 {:value form}

parser-type-spec ::= (integer integer-size) |
                     (string-type string-termination-spec)

integer-size ::= octet-count | :lenenc
 octet-count - read this many octets
 :lenenc - read a length-encoded integer
 :lenenc-null-ok - read a length-encoded integer, allowing integer to be NULL.

string-type ::= octets | string
 octets - '(vector (unsigned-byte 8))
 string - shorthand for octets transformed with #'babel:octets-to-string.
          NB: this is separate from the :transform option.

string-termination-spec ::= integer | :eof | :lenenc | :null | :null-eof
 integer - a specific length.
 :eof - read until the end of the packet.
 :lenenc - read a length-encoded integer first, then use that as the length.
 :lenenc-null-ok - read a length-encoded integer first; if not null then use that as the length.
 :null - read until a null octet is encountered.
 :null-eof - read until a null octet is encountered or we hit the end of the packet.
             Used to deal with a bug in some forms of the initial handshake packet.

eof-action ::= :error | :end
 :error - default; end-of-file signaled
 :end - stop parsing packet and return collected data

Bind - Bind the slot value to its name so later slots may use it.
Predicate - The provided form must return non-nil for this slot to be parsed as described.
Reduce - The parsed value is combined with a previously parsed value of the same name using the λ provided.
         The old value is passed as the first argument; the new value is passed as the second argument.
Transform - The parsed value is transformed using the provided λ.
Transient - The parsed value is not returned as a parsed value. It may be used internally if named.
Value - If present, the value parsed for this slot is expected to be equal to the value of the form.

The Lisp type specified by :TYPE may be omitted if it can be deduced from :MYSQL-TYPE. Generally, if
:TRANSFORM or :REDUCE are specified, you should specify a :TYPE.

Order of Operations:

• Predicate
• parse-from-stream
• Transform
• Value
• Reduce
• Bind
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slot meta-data
(defclass packet-slot ()
  ((name :type symbol
         :initarg :name
         :reader packet-slot-name)
   (bind :type boolean
         :initarg :bind
         :initform t
         :accessor packet-slot-bind)
   (eof  :type keyword
         :initarg :eof
         :initform :error
         :accessor packet-slot-eof)
   (predicate :initform nil
              :initarg :predicate
              :accessor packet-slot-predicate)
   (reduce :initform nil
           :initarg :reduce
           :accessor packet-slot-reduce)
   (transform :initform nil
              :initarg :transform
              :accessor packet-slot-transform)
   (transient :type boolean
              :initarg :transient
              :initform nil
              :accessor packet-slot-transient)
   (mysql-type :initarg :mysql-type
               :accessor packet-slot-mysql-type)
   (type :initarg :type
         :initform nil
         :accessor packet-slot-type)
   (value :initarg :value
          :initform nil
          :accessor packet-slot-value)))

(defun parse-slot (slot-specifier)
  (destructuring-bind (slot-name &rest slot-properties)
      slot-specifier
    (apply #'make-instance
           'packet-slot
           :name slot-name
           slot-properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packet struct
(defun emit-packet-slot-lisp-type (slotd optional)
  (destructuring-bind (mysql-type termination-spec)
      (packet-slot-mysql-type slotd)
    (cond
      ((packet-slot-type slotd))
      (t
       (let ((base-type
               (ecase mysql-type
                 (integer
                  (cond
                    ((typep termination-spec 'integer)
                     `(integer 0 ,(1- (ash 1 (* 8 termination-spec)))))
                    (t 'integer)))
                 (octets '(vector (unsigned-byte 8)))
                 (string 'string))))
         (if (or optional
                 (packet-slot-predicate slotd)
                 (eq termination-spec :lenenc-or-null))
             `(or ,base-type null)
             base-type))))))

(defun emit-packet-struct (struct-name slotds)
  `(defstruct ,struct-name
     ,@(loop for slotd in slotds
             for optional = (eq (packet-slot-eof slotd) :end)
               then (or optional (eq (packet-slot-eof slotd) :end))
             unless (or (packet-slot-transient slotd)
                        (member (packet-slot-name slotd) done))
               collect `(,(packet-slot-name slotd)
                         nil
                         :type ,(emit-packet-slot-lisp-type slotd optional))
             collect (packet-slot-name slotd) into done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser logic
(defun emit-packet-parser-slot-reader (slotd stream locals)
  (destructuring-bind (mysql-type termination-spec)
      (packet-slot-mysql-type slotd)
    (ecase mysql-type
      (integer
       (cond
         ((or (typep termination-spec 'integer)
              (member termination-spec locals))
          `(read-fixed-length-integer ,termination-spec ,stream))
         ((eq termination-spec :lenenc)
          `(read-length-encoded-integer ,stream))
         ((eq termination-spec :lenenc-null-ok)
          `(read-length-encoded-integer ,stream :null-ok t))
         (t (error (make-condition 'bad-mysql-type-spec
                                   :text (format nil "Unexpected termination type for integer: ~A." termination-spec))))))
      ((octets string)
       (let ((parser
               (cond
                 ((or (typep termination-spec 'integer)
                      (member termination-spec locals))
                  `(read-fixed-length-octets ,termination-spec ,stream))
                 (t
                  (ecase termination-spec
                    (:eof
                     `(read-rest-of-packet-octets ,stream))
                    (:lenenc
                     `(read-length-encoded-octets ,stream))
                    (:lenenc-null-ok
                     `(read-length-encoded-octets ,stream :null-ok t))
                    (:null
                     `(read-null-terminated-octets ,stream))
                    (:null-eof
                     `(read-null-terminated-octets ,stream nil)))))))
         (if (eq mysql-type 'string)
             (with-gensyms (octets)
               `(let ((,octets ,parser))
                  (when ,octets
                    (babel:octets-to-string ,octets))))
             parser))))))

(defun emit-packet-parser-slot (parser-name slotd stream locals)
  (declare (ignorable parser-name))
  (with-gensyms (value)
    (let ((body (emit-packet-parser-slot-reader slotd stream locals)))
      (when (packet-slot-transform slotd)
        (setf body `(funcall ,(packet-slot-transform slotd) ,body)))
      (when (packet-slot-value slotd)
        (setf body
              `(let ((,value ,body))
                 (assert (equal ,(packet-slot-value slotd) ,value))
                 ,value)))
      (when (packet-slot-reduce slotd)
        (setf body `(funcall ,(packet-slot-reduce slotd)
                             ,(packet-slot-name slotd)
                             ,body)))
      (when (packet-slot-bind slotd)
        (setf body `(setf ,(packet-slot-name slotd) ,body)))

      (when (packet-slot-predicate slotd)
        (setf body
              `(when ,(packet-slot-predicate slotd)
                 ,body)))
      (when (eq (packet-slot-eof slotd) :end)
        (setf body
              `(handler-case
                   ,body
                 (end-of-file () (return-from ,parser-name (values))))))
      body)))

(defun emit-packet-parser (parser-name constructor-name slot-descriptors)
  (with-gensyms (stream #|local-bind-args|#)
    `(defun ,parser-name (,stream)
       (let (,@(loop for slotd in slot-descriptors
                  unless (member (packet-slot-name slotd) done)
                  when (packet-slot-bind slotd)
                  collect (packet-slot-name slotd)
                  collect (packet-slot-name slotd) into done))
         (block ,parser-name
           ,@(loop for slotd in slot-descriptors
                collect (emit-packet-parser-slot parser-name slotd stream locals)
                when (packet-slot-bind slotd)
                collect (packet-slot-name slotd) into locals))
         (,constructor-name
          ,@(loop for slotd in slot-descriptors
               unless (or (packet-slot-transient slotd)
                          (member (packet-slot-name slotd) done))
               collect (kintern "~A" (packet-slot-name slotd))
               and collect (packet-slot-name slotd)
               collect (packet-slot-name slotd) into done))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point macro
(defmacro define-packet (name slots)
  (let ((parser-name (fintern "~A-~A" 'parse name))
        (struct-name (fintern "~A-~A" name 'packet))
        (struct-constructor (fintern "~A-~A-~A" 'make name 'packet))
        (slot-descriptors (mapcar #'parse-slot slots)))
      `(progn
         ;; Define a struct to hold non-transient data
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,(emit-packet-struct struct-name slot-descriptors))
         ;; Define a parser to parse a payload of this form and populate the struct
         ,(emit-packet-parser parser-name struct-constructor slot-descriptors)
         ;; Define a writer to generate a packet payload of this type from the struct
         #| Implement writer here (only needed for servers, not for mere clients) |#
         ;;
         ',name)))
