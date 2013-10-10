;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Scott McKay                                     ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)


;;; Optimized fixnum arithmetic

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter +optimize-default+     '(optimize (speed 1) (safety 3) (debug 3))
  "Compiler optimization settings for safe, debuggable code.")
(defparameter +optimize-fast-unsafe+ '(optimize (speed 3) (safety 0) (debug 0))
  "Compiler optimization settings for fast, unsafe, hard-to-debug code.")

)       ;eval-when


(defmacro i+ (&rest fixnums)
  `(the fixnum (+ ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i- (number &rest fixnums)
  `(the fixnum (- (the fixnum ,number) ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i* (&rest fixnums)
  `(the fixnum (* ,@(loop for n in fixnums collect `(the fixnum ,n)))))

(defmacro i= (&rest fixnums)
  `(= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i< (&rest fixnums)
  `(< ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i<= (&rest fixnums)
  `(<= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i> (&rest fixnums)
  `(> ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro i>= (&rest fixnums)
  `(>= ,@(loop for n in fixnums collect `(the fixnum ,n))))

(defmacro iash (value count)
  `(the fixnum (ash (the fixnum ,value) (the fixnum ,count))))

(defmacro ilogior (&rest fixnums)
  (if (cdr fixnums)
    `(the fixnum (logior (the fixnum ,(car fixnums))
                         ,(if (cddr fixnums)
                            `(ilogior ,@(cdr fixnums))
                            `(the fixnum ,(cadr fixnums)))))
    `(the fixnum ,(car fixnums))))

(defmacro ilogand (&rest fixnums)
  (if (cdr fixnums)
    `(the fixnum (logand (the fixnum ,(car fixnums))
                         ,(if (cddr fixnums)
                            `(ilogand ,@(cdr fixnums))
                            `(the fixnum ,(cadr fixnums)))))
    `(the fixnum ,(car fixnums))))

(define-modify-macro iincf (&optional (delta 1)) i+)
(define-modify-macro idecf (&optional (delta 1)) i-)

(defmacro ildb (bytespec value)
  `(the fixnum (ldb ,bytespec (the fixnum ,value))))


;;; Managing symbols

(defmacro with-gensyms ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (b) `(,b (gensym ,(string b)))) bindings)
     ,@body))

(defun make-lisp-symbol (string)
  "Intern a string of the 'package:string' and return the symbol."
  (let* ((string (string string))
         (colon  (position #\: string))
         (pkg    (if colon (subseq string 0 colon) "KEYWORD"))
         (sym    (if colon (subseq string (+ colon 1)) string)))
    (intern sym pkg)))

(defun fintern (format-string &rest format-args)
  "Interns a new symbol in the current package."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args))))

(defun kintern (format-string &rest format-args)
  "Interns a new symbol in the keyword package."
  (declare (dynamic-extent format-args))
  (intern (nstring-upcase (apply #'format nil format-string format-args)) "KEYWORD"))

(defun keywordify (x)
  "Given a symbol designator 'x', return a keyword whose name is 'x'.
   If 'x' is nil, this returns nil."
  (check-type x (or string symbol null))
  (cond ((null x) nil)
        ((keywordp x) x)
        ((symbolp x) (keywordify (symbol-name x)))
        ((zerop (length x)) nil)
        ((string-not-equal x "nil")
         (intern (string-upcase x) (find-package "KEYWORD")))
        (t nil)))


;;; with-prefixed-accessors

(defmacro with-prefixed-accessors (names (prefix object) &body body)
  `(with-accessors (,@(loop for name in names
                            collect `(,name ,(fintern "~A~A" prefix name))))
       ,object
     ,@body))

;;; Functional programming, please

(defun curry (function &rest args)
  "Returns a function that applies 'function' to 'args', plus any
   additional arguments given at the call site."
  (if (and args (null (cdr args)))                      ;fast test for length = 1
    (let ((arg (car args)))
      #'(lambda (&rest more-args)
          (apply function arg more-args)))
    #'(lambda (&rest more-args)
        (apply function (append args more-args)))))

(define-compiler-macro curry (&whole form function &rest args &environment env)
  (declare (ignore env))
  (if (and (listp function)
           (eq (first function) 'function)
           (symbolp (second function))
           (and args (null (cdr args))))
    `#'(lambda (&rest more-args)
         (apply ,function ,(car args) more-args))
    form))

;;; Decimal Parsing

(defun parse-decimal (str)
  ;; Look into replacing this with a library.
  (assert (and
           (let ((x (aref str 0)))
             (or (char<= #\0 x #\9)
                 (char=  x #\-)))
           (every #'(lambda (x) (or (char<= #\0 x #\9)
                                    (char= x #\.)))
                  (subseq str 1))
           (< (count #\. str) 2)))
  (let ((start 0)
        (sign 1)
        found-decimal)
    (when (string-prefix-p "-" str)
      (setq start 1
            sign -1))
    (loop
      for i from start below (length str)
      for c = (aref str i) then (aref str i)
      for denominator = 0 then (if found-decimal (1+ denominator) denominator)
      if (char= c #\.)
        do (setq found-decimal t)
      else
        collect c into numerator
      finally
         (return (* sign
                    (/ (parse-integer (coerce numerator 'string))
                       (expt 10 denominator)))))))

(defun %denominator-divisible-by-2-or-5-only (x)
  "Helper function used in the definition of type DECIMAL-NUMBER.

  Return T if the denominator of x has only 2 and/or 5 as factors,
  otherwise return NIL."
  (let ((twos 0)
        (fives 0))
    (values (or (zerop x)
                (let ((x (denominator x)))
                  (loop while (evenp x)
                        do (setq x (ash x -1))
                           (incf twos))
                  (loop with r = 0
                        while (and (> x 1) (zerop r))
                        do (multiple-value-setq (x r) (truncate x 5))
                        when (zerop r)
                          do (incf fives)
                        finally (return (zerop r)))))
            twos fives)))

(defun write-decimal-to-string (value)
  ;; Collect factors
  (multiple-value-bind (decimalp twos fives)
      (%denominator-divisible-by-2-or-5-only value)
    (unless decimalp
      (error 'value-is-not-decimal :value value))
    (let ((n (numerator value))
          (d (denominator value)))
      ;; Adjust denominator
      (unless (= twos fives)
        (let ((multiple (expt (if (< twos fives) 2 5)
                              (abs (- twos fives)))))
          (setf n (* n multiple)
                d (* d multiple))))
      ;; Split into parts
      (multiple-value-bind
            (whole frac)
          (truncate (abs n) d)

        ;; Combine into string
        (apply #'concatenate
               'string
               (when (minusp value) "-")
               (princ-to-string whole)
               (unless (zerop frac)
                 `("."
                   ,(princ-to-string frac))))))))


;;; Portable floating point utilities

#+(or abcl allegro ccl cmu sbcl lispworks)
(defun single-float-bits (x)
  (declare (type single-float x))
  #+abcl    (system:single-float-bits x)
  #+allegro (multiple-value-bind (high low)
                (excl:single-float-to-shorts x)
              (declare (type (unsigned-byte 16) high low))
              (logior (ash high 16) low))
  #+ccl (ccl::single-float-bits x)
  #+cmu  (kernel:single-float-bits x)
  #+sbcl (sb-kernel:single-float-bits x)
  #+lispworks (lispworks-float:single-float-bits x))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun single-float-bits (x)
  (declare (type single-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
    (if (eql x 0.0f0) 0 #x-80000000)
    (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
        (integer-decode-float x)
      (assert (plusp lisp-significand))
      (let* ((significand lisp-significand)
             (exponent (+ lisp-exponent 23 127))
             (unsigned-result
              (if (plusp exponent)                      ;if not obviously denormalized
                (do () (nil)
                  (cond
                    ;; Special termination case for denormalized float number
                    ((zerop exponent)
                     ;; Denormalized numbers have exponent one greater than
                     ;; in the exponent field
                     (return (ash significand -1)))
                    ;; Ordinary termination case
                    ((>= significand (expt 2 23))
                     (assert (< 0 significand (expt 2 24)))
                     ;; Exponent 0 is reserved for denormalized numbers,
                     ;; and 255 is reserved for specials like NaN
                     (assert (< 0 exponent 255))
                     (return (logior (ash exponent 23)
                                     (logand significand (1- (ash 1 23))))))
                    (t
                     ;; Shift as necessary to set bit 24 of significand
                     (setq significand (ash significand 1)
                           exponent (1- exponent)))))
                (do () ((zerop exponent)
                        ;; Denormalized numbers have exponent one greater than
                        ;; the exponent field
                        (ash significand -1))
                  (unless (zerop (logand significand 1))
                    (warn "Denormalized '~S' losing bits in ~D" 'single-float-bits x))
                  (setq significand (ash significand -1)
                        exponent (1+ exponent))))))
        (ecase lisp-sign
          ((1)  unsigned-result)
          ((-1) (logior unsigned-result (- (expt 2 31)))))))))


#+(or abcl allegro ccl cmu sbcl lispworks)
(defun double-float-bits (x)
  (declare (type double-float x))
  #+abcl    (values (system:double-float-low-bits x)
                    (system:double-float-high-bits x))
  #+allegro (multiple-value-bind (us3 us2 us1 us0)
                (excl:double-float-to-shorts x)
              (logior (ash us1 16) us0)
              (logior (ash us3 16) us2))
  #+ccl  (multiple-value-bind (high low)
             (ccl::double-float-bits x)
           (values low high))
  #+cmu  (values (kernel:double-float-low-bits x)
                 (kernel:double-float-high-bits x))
  #+sbcl (values (sb-kernel:double-float-low-bits x)
                 (sb-kernel:double-float-high-bits x))
  #+lispworks (let ((bits (lispworks-float:double-float-bits x)))
                (values (logand #xffffffff bits)
                        (ash bits -32))))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun double-float-bits (x)
  (declare (type double-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
    (if (eql x 0.0d0) 0 #x-8000000000000000)
    (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
        (integer-decode-float x)
      (assert (plusp lisp-significand))
      (let* ((significand lisp-significand)
             (exponent (+ lisp-exponent 52 1023))
             (unsigned-result
              (if (plusp exponent)                      ;if not obviously denormalized
                (do () (nil)
                  (cond
                    ;; Special termination case for denormalized float number
                    ((zerop exponent)
                     ;; Denormalized numbers have exponent one greater than
                     ;; in the exponent field
                     (return (ash significand -1)))
                    ;; Ordinary termination case
                    ((>= significand (expt 2 52))
                     (assert (< 0 significand (expt 2 53)))
                     ;; Exponent 0 is reserved for denormalized numbers,
                     ;; and 2047 is reserved for specials like NaN
                     (assert (< 0 exponent 2047))
                     (return (logior (ash exponent 52)
                                     (logand significand (1- (ash 1 52))))))
                    (t
                     ;; Shift as necessary to set bit 53 of significand
                     (setq significand (ash significand 1)
                           exponent (1- exponent)))))
                (do () ((zerop exponent)
                        ;; Denormalized numbers have exponent one greater than
                        ;; the exponent field
                        (ash significand -1))
                  (unless (zerop (logand significand 1))
                    (warn "Denormalized '~S' losing bits in ~D" 'double-float-bits x))
                  (setq significand (ash significand -1)
                        exponent (1+ exponent))))))
        (let ((result
               (ecase lisp-sign
                 ((1)  unsigned-result)
                 ((-1) (logior unsigned-result (- (expt 2 63)))))))
          ;; Return the low bits and the high bits
          (values (logand #xffffffff result) (ash result -32)))))))


#+(or abcl allegro ccl cmu sbcl lispworks)
(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  #+abcl    (system:make-single-float bits)
  #+allegro (excl:shorts-to-single-float (ldb (byte 16 16) bits)
                                         (ldb (byte 16 0) bits))
  #+ccl  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+cmu  (kernel:make-single-float bits)
  #+sbcl (sb-kernel:make-single-float bits)
  #+lispworks (lispworks-float:make-single-float bits))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= bits #x-80000000) -0.0)
    (t
     (let* ((sign (ecase (ldb (byte 1 31) bits)
                    (0 1.0)
                    (1 -1.0)))
            (iexpt (ldb (byte 8 23) bits))
            (exponent (if (zerop iexpt)                 ;denormalized
                        -126
                        (- iexpt 127)))
            (mantissa (* (logior (ldb (byte 23 0) bits)
                                 (if (zerop iexpt) 0 (ash 1 23)))
                         (expt 0.5 23))))
       (* sign (expt 2.0 exponent) mantissa)))))


#+(or abcl allegro ccl cmu sbcl lispworks)
(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low)
           (type (signed-byte   32) high))
  #+abcl (system:make-double-float (logior (ash high 32) low))
  #+allegro (excl:shorts-to-double-float (ldb (byte 16 16) high)
                                         (ldb (byte 16 0) high)
                                         (ldb (byte 16 16) low)
                                         (ldb (byte 16 0) low))
  #+ccl  (ccl::double-float-from-bits (ilogand high #xffffffff) low)
  #+cmu  (kernel:make-double-float high low)
  #+sbcl (sb-kernel:make-double-float high low)
  #+lispworks (lispworks-float:make-double-float high low))

#-(or abcl allegro ccl cmu sbcl lispworks)
(defun make-double-float (low high)
  (declare (type (unsigned-byte 32) low)
           (type (signed-byte   32) high))
  (cond
    ;; IEEE float special cases
    ((and (zerop high) (zerop low)) 0.0d0)
    ((and (= high #x-80000000)
          (zerop low)) -0.0d0)
    (t
     (let* ((bits (logior (ash high 32) low))
            (sign (ecase (ldb (byte 1 63) bits)
                    (0 1.0d0)
                    (1 -1.0d0)))
            (iexpt (ldb (byte 11 52) bits))
            (exponent (if (zerop iexpt)                 ;denormalized
                        -1022
                        (- iexpt 1023)))
            (mantissa (* (logior (ldb (byte 52 0) bits)
                                 (if (zerop iexpt) 0 (ash 1 52)))
                         (expt 0.5d0 52))))
       (* sign (expt 2.0d0 exponent) mantissa)))))
