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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packet Definition Macrology

#||
Specs:

slot-specifier ::= (slot-name [[slot-options]])
slot-name ::= symbol
slot-options ::= {:bind boolean } |
                 {:eof eof-action} |
                 {:predicate form} |
                 {:reduce λ α′ α → β)
                 {:transform λ α → β} |
                 {:transient boolean} |
                 {:mysql-type parser-type-spec} |
                 {:value form}

parser-type-spec ::= (integer integer-size) |
                     (string-type string-termination-spec)

integer-size ::= byte-count | :lenenc
 byte-count - read this many bytes
 :lenenc - read a length-encoded integer

string-type ::= octets | string
 octets - '(vector (unsigned-byte 8))
 string - shorthand for octets transformed with #'babel:octets-to-string.
          NB: this is separate from the :transform option.

string-termination-spec ::= integer | :eof | :lenenc | :null | :null-eof
 integer - a specific length.
 :eof - read until the end of the packet.
 :lenenc - read a length-encoded integer first, then use that as the length.
 :null - read until a null byte is encountered.
 :null-eof - read until a null byte is encountered or we hit the end of the packet.
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

Order of Operations:

• Predicate
• parse-from-stream
• Transform
• Value
• Reduce
• Bind
• slot-bind (unless transient)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slot meta-data
(defclass packet-slot ()
  ((name :type symbol
         :initarg :name
         :reader packet-slot-name)
   (bind :type boolean
         :initarg :bind
         :initform nil
         :accessor packet-slot-bind)
   (eof  :type keyword
         :initarg :eof
         :initform :error
         :accessor packet-slot-eof)
   (predicate :type (or function null)
              :initform nil
              :initarg :predicate
              :accessor packet-slot-predicate)
   (reduce :type (or function null)
              :initform nil
           :initarg :reduce
           :accessor packet-slot-reduce)
   (transform :type (or function null)
              :initform nil
              :initarg :transform
              :accessor packet-slot-transform)
   (transient :type boolean
              :initarg :transient
              :initform nil
              :accessor packet-slot-transient)
   (mysql-type :initarg :mysql-type
               :accessor packet-slot-mysql-type)
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
(defun emit-packet-slot-lisp-type (slotd)
  (destructuring-bind (mysql-type termination-spec)
      (packet-slot-mysql-type slotd)
    (declare (ignore termination-spec))
    (let ((base-type
            (ecase mysql-type
              (integer 'integer)
              (octets '(vector (unsigned-byte 8)))
              (string 'string))))
      (if (packet-slot-predicate slotd)
          `(or ,base-type null)
          base-type))))

(defun emit-packet-struct (struct-name slotds)
  `(defstruct ,struct-name
     ,@(loop for slotd in slotds
             unless (or (packet-slot-transient slotd)
                        (member (packet-slot-name slotd) done))
               collect `(,(packet-slot-name slotd)
                         nil
                         :type ,(emit-packet-slot-lisp-type slotd))
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
          `(parse-fixed-length-integer ,stream ,termination-spec))
         ((eq termination-spec :lenenc)
          `(parse-length-encoded-integer ,stream))
         ;; asedeno-TODO: real conditions
         (t (error "unexpected termination type for integer."))))
      ((octets string)
       (let ((parser
               (cond
                 ((or (typep termination-spec 'integer)
                      (member termination-spec locals))
                  `(parse-fixed-length-string ,stream ,termination-spec))
                 (t
                  (ecase termination-spec
                    (:eof
                     `(parse-rest-of-packet-string ,stream))
                    (:lenenc
                     `(parse-length-encoded-string ,stream))
                    (:null
                     `(parse-null-terminated-string ,stream))
                    (:null-eof
                     `(parse-null-terminated-string ,stream nil)))))))
         (if (eq mysql-type 'string)
             `(babel:octets-to-string ,parser)
             parser))))))

(defun emit-packet-parser-slot (parser-name struct-name slotd stream packet locals)
  (declare (ignorable parser-name))
  (let ((struct-slot (fintern "~A-~A" struct-name (packet-slot-name slotd))))
    (with-gensyms (value)
      (let ((body `(let ((,value ,(emit-packet-parser-slot-reader slotd stream locals)))
                     ,@(when (and (packet-slot-transient slotd) (not (packet-slot-bind slotd)))
                         `((declare (ignorable ,value))))
                     ,@(when (packet-slot-transform slotd)
                         `((setf ,value (funcall ,(packet-slot-transform slotd) ,value))))
                     ,@(when (packet-slot-value slotd)
                         `((assert (equal ,(packet-slot-value slotd) ,value))))
                     ,@(when (packet-slot-reduce slotd)
                         `((setf ,value (funcall ,(packet-slot-reduce slotd) (,struct-slot ,packet) ,value))))
                     ,@(unless (packet-slot-transient slotd)
                         `((setf (,struct-slot ,packet) ,value)))
                     ,@(when (packet-slot-bind slotd)
                         `((setf ,(packet-slot-name slotd) ,value))))))
        (when (packet-slot-predicate slotd)
          (setf body
                `(when ,(packet-slot-predicate slotd)
                   ,body)))
        (when (eq (packet-slot-eof slotd) :end)
          (setf body
                `(handler-case
                     ,body
                   (end-of-file () (return-from ,parser-name ,packet)))))
        body))))

(defun emit-packet-parser (parser-name struct-name constructor-name slot-descriptors)
  (with-gensyms (stream packet #|local-bind-args|#)
    `(defun ,parser-name (payload)
       (block ,parser-name
         (let ((,stream (flexi-streams:make-in-memory-input-stream payload))
               (,packet (,constructor-name))
               ,@(loop for slotd in slot-descriptors
                       when (packet-slot-bind slotd)
                         collect (packet-slot-name slotd)))
           ,@(loop for slotd in slot-descriptors
                   collect (emit-packet-parser-slot parser-name struct-name slotd stream packet locals)
                   when (packet-slot-bind slotd)
                     collect (packet-slot-name slotd) into locals)
           ,packet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point macro
(defmacro define-packet (name slots)
  (let ((parser-name (fintern "PARSE-~A" name))
        (struct-name (fintern "~A-PACKET" name))
        (struct-constructor (fintern "MAKE-~A-PACKET" name))
        (slot-descriptors (mapcar #'parse-slot slots)))
      `(progn
         ;; Define a struct to hold non-transient data
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,(emit-packet-struct struct-name slot-descriptors))
         ;; Define a parser to parse a payload of this form and populate the struct
         ,(emit-packet-parser parser-name struct-name struct-constructor slot-descriptors)
         ;; Define a writer to generate a packet payload of this type from the struct
         #| Implement writer here |#
         ;;
         ',name)))
