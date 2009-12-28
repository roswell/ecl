;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;; CMPSYSFUN   Database for system functions.
;;;
;;; Copyright (c) 2003, Juan Jose Garcia Ripoll
;;; Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;    Copying of this file is authorized to users who have executed the true
;;;    and proper "License Agreement for ECoLisp".
;;;
;;; DATABASE OF FUNCTION PROCLAMATIONS AND INLINE EXPANSIONS
;;;
;;; What follows is the complete list of function type proclamations for the
;;; most important functions in the ECL core library, together with some useful
;;; inline expansions.
;;;
;;; The function proclamations are created with PROCLAIM-FUNCTION, as in
;;;
;;;	(PROCLAIM-FUNCTION function-name ([arg-type]*) return-type
;;;		&rest {:no-sp-change|:pure|:reader|:no-side-effects})
;;;
;;; with the following interpretation: ARG-TYPE and RETURN-TYPE denote the most
;;; general types for the input and output values of this function. If the
;;; compiler detects that some of the values passed to this function does not
;;; match these types, it will generate an error. In addition to this, ECL
;;; contemplates different function properties:
;;;
;;; :NO-SP-CHANGE indicates that the function does not change the value of any
;;;        special variable, and it is used to perform code transformations.
;;;
;;; :NO-SIDE-EFFECTS is slightly stronger, as it indicates that the function
;;;        does not change variables or the content of objects in the
;;;        thread environment. Note the following:
;;;
;;;    - Allocating memory, creating objects, etc is not considered a side
;;;      effect, as it does not affect the code flow.
;;;    - Similarly, signalling errors is not considered a side effect.
;;;    - The environment may be changed by other threads. This is taken
;;;      into account (see below).
;;;
;;; :READER indicates that the function not only has no side effects, but its
;;;        value depends only on its arguments. However, :READER specifies that
;;;        the arguments are mutable.
;;;
;;; :PURE is the strictest class of functions. They have no side effects, the
;;;        output only depends on the arguments, the arguments are inmutable
;;;        objects and the function call can be optimized away when the
;;;        arguments are constant.
;;;
;;; Inline expansions, on the other hand, have the following syntax
;;;
;;;	(DEF-INLINE function-name kind ([arg-type]*) return-rep-type
;;;		expansion-string)
;;;
;;; Here, ARG-TYPE is the list of argument types belonging to the lisp family,
;;; while RETURN-REP-TYPE is a representation type, i.e. the C type of the
;;; output expression. EXPANSION-STRING is a C/C++ expression template, like the
;;; ones used by C-INLINE. Finally, KIND can be :ALWAYS, :SAFE or :UNSAFE,
;;; depending on whether the inline expression should be applied always, in safe
;;; or in unsafe compilation mode, respectively.
;;;

(in-package "COMPILER")

(defmacro proclaim-function (&whole form name arg-types return-type &rest properties)
  (when (get-sysprop name 'proclaimed-arg-types)
    (warn "Duplicate proclamation for ~A" name))
  (unless (or (equal arg-types '(*)))
    (put-sysprop name 'proclaimed-arg-types arg-types))
  (when (and return-type (not (eq 'T return-type)))
    (put-sysprop name 'proclaimed-return-type return-type))
  (loop for p in properties
     do (case p
          (:no-sp-change
           (put-sysprop name 'no-sp-change t))
          ((:predicate :pure)
           (put-sysprop name 'pure t)
           (put-sysprop name 'no-side-effects t))
          ((:no-side-effects :reader)
           (put-sysprop name 'no-side-effects t))
          (otherwise
           (error "Unknown property ~S in function proclamation ~S" p form))))
  (rem-sysprop name ':inline-always)
  (rem-sysprop name ':inline-safe)
  (rem-sysprop name ':inline-unsafe)
  nil)

(defmacro def-inline (name safety arg-types return-rep-type expansion
                      &key (one-liner t) (exact-return-type nil)
		      &aux arg-rep-types)
  (setf safety
	(case safety
	  (:unsafe :inline-unsafe)
	  (:safe :inline-safe)
	  (:always :inline-always)
	  (t (error "In DEF-INLINE, wrong value of SAFETY"))))
  (setf arg-rep-types
	(mapcar #'(lambda (x) (if (eq x '*) x (lisp-type->rep-type x)))
		arg-types))
  (when (eq return-rep-type t)
    (setf return-rep-type :object))
  (let* ((return-type (if (and (consp return-rep-type)
                               (eq (first return-rep-type) 'values))
                          t
                          (rep-type->lisp-type return-rep-type)))
         (inline-info
          (make-inline-info :name name
                            :arg-rep-types arg-rep-types
                            :return-rep-type return-rep-type
                            :return-type return-type
                            :arg-types arg-types
                            :exact-return-type exact-return-type
                            ;; :side-effects (not (get-sysprop name 'no-side-effects))
                            :one-liner one-liner
                            :expansion expansion))
         (previous (get-sysprop name safety)))
    #+(or)
    (loop for i in previous
       when (and (equalp (inline-info-arg-types i) arg-types)
                 (not (equalp return-type (inline-info-return-type i))))
       do (format t "~&;;; Redundand inline definition for ~A~&;;; ~<~A~>~&;;; ~<~A~>"
                  name i inline-info))
    (put-sysprop name safety (cons inline-info previous)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUXILIARY TYPES
;;

(deftype string-designator () '(or string symbol character))
(deftype natural () '(integer 0 *))
(deftype function-name () '(or list symbol))
(deftype function-designator () '(or symbol function))
(deftype extended-function-designator () '(or function-name function))
(deftype environment () 'list)
(deftype type-specifier () '(or symbol class list))
(deftype gen-bool () "Generalized boolean type" 't)
(deftype format-control () "Format control for FORMAT" '(or string function))
(deftype restart-designator () '(or (and symbol (not (member nil))) restart))
(deftype package-designator () '(or string-designator package))
(deftype byte-specifier () '(cons unsigned-byte unsigned-byte))
(deftype character-designator () '(or string-designator character))
(deftype radix () '(integer 2 36))
(deftype digit-weight () '(integer 0 35))
(deftype character-code () '(integer 0 #.(1- char-code-limit)))
(deftype tree () 't)
(deftype association-list () 'list)
(deftype bit-array () '(array bit))
(deftype pathname-designator () '(or string pathname stream))
(deftype pathname-host () '(or string list (member nil :unspecific)))
(deftype pathname-device () '(or string (member nil :unspecific)))
(deftype pathname-directory () '(or string list (member :wild :unspecific)))
(deftype pathname-name () '(or string (member nil :wild :unspecific)))
(deftype pathname-type () '(or string (member nil :wild :unspecific)))
(deftype pathname-version () '(or unsigned-byte (member nil :wild :newest :unspecific)))
(deftype universal-time () 'unsigned-byte)
(deftype time-zone () '(rational -24 24))
(deftype stream-designator () '(or stream (member t nil)))
(deftype file-position-designator () '(or unsigned-byte (member :start :end)))
(deftype external-file-format () '(or symbol list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL FUNCTION DECLARATIONS
;;;
;;;
;;; ANSI SECTIONS
;;;
;;; 3. EVALUATION AND COMPILATION
;;;

(proclaim-function compile (function-name &optional (or list function))
                   (values (or function-name function) gen-bool gen-bool))
(proclaim-function compiler-macro-function (function-name &optional environment)
                   function)
(proclaim-function constantp (t &optional environment) gen-bool :no-side-effects)
(proclaim-function eval (t) (values &rest t))
(proclaim-function macro-function (symbol &optional environment) function)
(proclaim-function macroexpand (t &optional environment) (values t gen-bool))
(proclaim-function macroexpand-1 (t &optional environment) (values t gen-bool))
(proclaim-function proclaim (list) (values &rest t))
(proclaim-function special-operator-p (symbol) gen-bool :pure)

;; ECL extensions:
(proclaim-function si:specialp (symbol) gen-bool :predicate)


;;;
;;; 4. TYPES AND CLASSES
;;;

(proclaim-function coerce (t type-specifier) t)
(proclaim-function subtypep (type-specifier type-specifier &optional environment)
                   (values gen-bool gen-bool))
(proclaim-function type-of (t) type-specifier)
(proclaim-function typep (t type-specifier &optional environment) gen-bool)

; Slot accessors:
; (proclaim-function type-error-datum (condition) t)
; (proclaim-function type-error-expected-type (condition) t)


;;;
;;; 5. DATA AND CONTROL FLOW
;;;

(proclaim-function apply (function-designator &rest t) (values &rest t))
(proclaim-function funcall (function-designator &rest t) (values &rest t))
(proclaim-function fdefinition (function-name) (or list function (member 'SPECIAL)))
(proclaim-function fboundp (function-name) gen-bool :no-side-effects)
(proclaim-function fmakunbound (function-name) function-name)
(proclaim-function function-lambda-expression (function) (values list gen-bool t))
(proclaim-function functionp (t) gen-bool :pure)
(proclaim-function compiled-function-p (t) gen-bool :pure)
(proclaim-function not (t) boolean :pure)
(proclaim-function eq (t t) gen-bool :pure)
(proclaim-function eql (t t) gen-bool :pure)
(proclaim-function equal (t t) gen-bool :pure)
(proclaim-function equalp (t t) gen-bool :pure)
(proclaim-function identity (t) t :no-side-effects)
(proclaim-function complement (function) function)
(proclaim-function constantly (t) function)
(proclaim-function every (function sequence &rest sequence) gen-bool)
(proclaim-function some (function sequence &rest sequence) t)
(proclaim-function notevery (function sequence &rest sequence) gen-bool)
(proclaim-function notany (function sequence &rest sequence) gen-bool)
(proclaim-function values-list (list) (values &rest t))
(proclaim-function get-setf-expansion (t &optional enviroment)
                   (values t t t t t))

;; ECL extensions

(proclaim-function si:fset (function-name function &optional gen-bool t) function)
(proclaim-function si:clear-compiler-properties (function-name) t)
(proclaim-function si:compiled-function-name (function) function-name)
(proclaim-function si:compiled-function-block (function) si::codeblock)
(proclaim-function si:compiled-function-file (function) t)

(proclaim-function si:ihs-top () si::index)
(proclaim-function si:ihs-fun (si::index) (or null function-designator))
(proclaim-function si:ihs-env (si::index) t)
(proclaim-function si:frs-top () si::index)
(proclaim-function si:frs-bds (si::index) si::index)
(proclaim-function si:frs-tag (si::index) t)
(proclaim-function si:frs-ihs (si::index) si::index)
(proclaim-function si:bds-top () si::index)
(proclaim-function si:bds-var (si::index) symbol)
(proclaim-function si:bds-val (si::index) t)
(proclaim-function si:sch-frs-base (si::index si::index) (or null si::index))


;;;
;;; 7. OBJECTS
;;;

(proclaim-function ensure-generic-function (function-name &rest t) generic-function)
(proclaim-function slot-boundp (si::instance symbol) gen-bool)
(proclaim-function slot-exists-p (si::instance symbol) gen-bool)
(proclaim-function slot-makunbound (si::instance symbol) si::instance)
(proclaim-function slot-value (si::instance symbol) t)
(proclaim-function make-load-form-saving-slots (t &rest t) (values t t))
(proclaim-function find-class (symbol &optional environment t) (or class null))
(proclaim-function class-of (t) class :no-side-effects)

;; Slot accessors:
; (proclaim-function unbound-slot-instance (condition) si::instance :predicate)


;;;
;;; 8. STRUCTURES
;;;

(proclaim-function copy-structure (t) t)

;; ECL extensions
(proclaim-function si:make-structure (t &rest t) structure-object)
(proclaim-function si:structure-name (structure-object) symbol :reader)
(proclaim-function si:structure-ref (structure-object t fixnum) t :reader)
(proclaim-function si:structure-set (structure-object t fixnum t) t)
(proclaim-function si:structurep (t) gen-bool :predicate)
(proclaim-function si:structure-subtype-p (t t) gen-bool :predicate)


;;;
;;; 9. CONDITIONS
;;;

(proclaim-function error (t &rest t) (values))
;; FIXME! It is not clear from the specification whether CERROR actually
;; returns values. However ECL is actually using the fact that it returns
;; the value from CONTINUE.
(proclaim-function cerror (format-control t &rest t) (values &rest t))
(proclaim-function invalid-method-error (method format-control &rest t) (values))
(proclaim-function method-combination-error (method format-control &rest t) (values))
(proclaim-function signal (t &rest t) null)
(proclaim-function warn (t &rest t) null)
(proclaim-function invoke-debugger (condition) (values))
(proclaim-function break (&optional format-control &rest t) null)
(proclaim-function make-condition (type-specifier &rest t) condition)
(proclaim-function compute-restarts (&optional condition) list)
(proclaim-function find-restart (restart-designator &optional condition) restart)
(proclaim-function invoke-restart (restart-designator &rest t) (values &rest t))
(proclaim-function invoke-restart-interactively (restart-designator) (values &rest t))
(proclaim-function abort (&optional condition) (values))
(proclaim-function continue (&optional condition) null)
(proclaim-function muffle-warning (&optional condition) (values))
(proclaim-function store-value (value &optional condition) null)
(proclaim-function use-value (value &optional condition) null)

;; Slot accessors:
;; (proclaim-function cell-error-name (cell-error) t)
;; (proclaim-function simple-condition-format-control (simple-condition) t)
;; (proclaim-function simple-condition-format-arguments (simple-condition) t)
;; (proclaim-function restart-name (restart) t)

;; ECL extensions
(proclaim-function ext:catch-signal (fixnum gen-bool) null)


;;;
;;; 10. SYMBOLS
;;;

(proclaim-function symbolp (t) gen-bool :pure)
(proclaim-function keywordp (t) gen-bool :pure)
(proclaim-function make-symbol (string) symbol)
(proclaim-function copy-symbol (symbol &optional gen-bool) symbol)
(proclaim-function gensym (&optional (or string natural)) symbol)
(proclaim-function gentemp (&optional string package-designator) symbol)
(proclaim-function symbol-function (symbol) (or list (member 'special) function))
(proclaim-function symbol-name (symbol) string :pure)
(proclaim-function symbol-package (symbol) (or package null) :reader)
(proclaim-function symbol-plist (symbol) list :reader)
(proclaim-function symbol-value (symbol) t :reader)
(proclaim-function get (symbol t &optional t) t :no-side-effects)
(proclaim-function remprop (symbol t) gen-bool)
(proclaim-function boundp (symbol) gen-bool :no-side-effects)
(proclaim-function makunbound (symbol) symbol)
(proclaim-function set (symbol t) symbol)

;; ECL extensions:
(proclaim-function si:*make-special (symbol) symbol)
(proclaim-function si:*make-constant (symbol t) symbol)
(proclaim-function si:put-f (list t t) list)
(proclaim-function si:rem-f (list t) boolean)
(proclaim-function si:set-symbol-plist (symbol t) t)
(proclaim-function si:putprop (symbol t t) t)
(proclaim-function si:put-sysprop (t t t) t)
(proclaim-function si:get-sysprop (t t t) t)
(proclaim-function si:rem-sysprop (t t) t)


;;;
;;; 11. PACKAGES
;;;

(proclaim-function export (list &optional package) t)
(proclaim-function find-symbol (string &optional package-designator)
                   (values symbol symbol))
(proclaim-function find-package (package-designator) (or package null))
(proclaim-function find-all-symbols (string) list)
(proclaim-function import (list &optional package-designator) t)
(proclaim-function list-all-packages () list)
(proclaim-function rename-package (package-designator package-designator
                                   &optional list) package)
(proclaim-function shadow (list &optional package-designator) t)
(proclaim-function shadowing-import (list &optional package-designator) t)
(proclaim-function delete-package (package-designator) gen-bool)
(proclaim-function make-package (string-designator &rest t) package)
(proclaim-function unexport (list &optional package-designator) t)
(proclaim-function unintern (symbol &optional package-designator) gen-bool)
(proclaim-function unuse-package (list &optional package-designator) t)
(proclaim-function use-package (list &optional package-designator) t)
(proclaim-function intern (string &optional package-designator) (values symbol symbol))
(proclaim-function package-name (package-designator) (or string null) :reader)
(proclaim-function package-nicknames (package-designator) list :reader)
(proclaim-function package-shadowing-symbols (package-designator) list :reader)
(proclaim-function package-use-list (package-designator) list :reader)
(proclaim-function package-used-by-list (package-designator) list :reader)
(proclaim-function packagep (t) gen-bool :pure)

;; Slot accessor:
;; (proclaim-function package-error-package (condition) package)

;; ECL extensions
(proclaim-function si:select-package (package-designator) package)
(proclaim-function si:package-hash-tables (package-designator)
                   (values hash-table hash-table list) :reader)
(proclaim-function si:package-lock (package-designator gen-bool) package)


;;;
;;; 12. NUMBERS
;;;

(proclaim-function = (number &rest number) gen-bool :pure)
(proclaim-function /= (number &rest number) gen-bool :pure)
(proclaim-function < (real &rest real) gen-bool :pure)
(proclaim-function > (real &rest real) gen-bool :pure)
(proclaim-function <= (real &rest real) gen-bool :pure)
(proclaim-function >= (real &rest real) gen-bool :pure)
(proclaim-function max (real &rest real) real :pure)
(proclaim-function min (real &rest real) real :pure)
(proclaim-function minusp (real) gen-bool :pure)
(proclaim-function plusp (real) gen-bool :pure)
(proclaim-function zerop (number) gen-bool :pure)
(proclaim-function floor (real &optional real) (values integer real) :pure)
(proclaim-function ceiling (real &optional real) (values integer real) :pure)
(proclaim-function truncate (real &optional real) (values integer real) :pure)
(proclaim-function round (real &optional real) (values integer real) :pure)
(proclaim-function ffloor (real &optional real) (values float real) :pure)
(proclaim-function fceiling (real &optional real) (values float real) :pure)
(proclaim-function ftruncate (real &optional real) (values float real) :pure)
(proclaim-function fround (real &optional real) (values float real) :pure)
(proclaim-function cos (number) number :pure)
(proclaim-function sin (number) number :pure)
(proclaim-function tan (number) number :pure)
(proclaim-function cosh (number) number :pure)
(proclaim-function sinh (number) number :pure)
(proclaim-function tanh (number) number :pure)
(proclaim-function acos (number) number :pure)
(proclaim-function asin (number) number :pure)
(proclaim-function atan (number &optional real) number :pure)
(proclaim-function acosh (number) number :pure)
(proclaim-function asinh (number) number :pure)
(proclaim-function atanh (number) number :pure)
(proclaim-function * (&rest number) number :pure)
(proclaim-function + (&rest number) number :pure)
(proclaim-function - (&rest number) number :pure)
(proclaim-function / (&rest number) number :pure)
(proclaim-function 1+ (number) number :pure)
(proclaim-function 1- (number) number :pure)
(proclaim-function abs (number) (real 0 *) :pure)
(proclaim-function evenp (integer) gen-bool :pure)
(proclaim-function oddp (integer) gen-bool :pure)
(proclaim-function exp (number) number :pure)
(proclaim-function expt (number number) number :pure)
(proclaim-function gcd (&rest integer) unsigned-byte :pure)
(proclaim-function lcm (&rest integer) unsigned-byte :pure)
(proclaim-function log (number &optional number) number :pure)
(proclaim-function mod (real real) real :pure)
(proclaim-function rem (real real) real :pure)
(proclaim-function signum (number) number :pure)
(proclaim-function sqrt (number) number :pure)
(proclaim-function isqrt (unsigned-byte) unsigned-byte :pure)
(proclaim-function make-random-state (&optional (or random-state (member nil t)))
                   random-state)
(proclaim-function random ((or (integer 0 *) (float 0 *))
                           &optional random-state)
                   (or (integer 0 *) (float 0 *)))
(proclaim-function random-state-p (t) gen-bool :pure)
(proclaim-function numberp (t) gen-bool :pure)
(proclaim-function cis (real) complex :pure)
(proclaim-function complex (real &optional real) number :pure)
(proclaim-function complexp (t) gen-bool :pure)
(proclaim-function conjugate (number) number :pure)
(proclaim-function phase (number) number :pure)
(proclaim-function realpart (number) real :pure)
(proclaim-function imagpart (number) real :pure)
(proclaim-function upgraded-complex-part-type
                   (type-specifier &optional environment)
                   type-specifier)
(proclaim-function realp (t) gen-bool :pure)
(proclaim-function numerator (rational) integer :pure)
(proclaim-function denominator (rational) unsigned-byte :pure)
(proclaim-function rational (real) rational :pure)
(proclaim-function rationalize (real) rational :pure)
(proclaim-function rationalp (t) gen-bool :pure)
(proclaim-function ash (integer integer) integer :pure)
(proclaim-function integer-length (integer) unsigned-byte :pure)
(proclaim-function integerp (t) gen-bool :pure)
(proclaim-function parse-integer (string &rest t) (values integer si::index))
(proclaim-function boole ((integer 0 15) integer integer) integer :pure)
(proclaim-function logand (&rest integer) integer :pure)
(proclaim-function logandc1 (integer integer) integer :pure)
(proclaim-function logandc2 (integer integer) integer :pure)
(proclaim-function logeqv (&rest integer) integer :pure)
(proclaim-function logior (&rest integer) integer :pure)
(proclaim-function lognand (integer integer) integer :pure)
(proclaim-function lognor (integer integer) integer :pure)
(proclaim-function lognot (integer) integer :pure)
(proclaim-function logorc1 (integer integer) integer :pure)
(proclaim-function logorc2 (integer integer) integer :pure)
(proclaim-function logxor (&rest integer) integer :pure)
(proclaim-function logbitp (unsigned-byte integer) gen-bool :pure)
(proclaim-function logcount (integer) unsigned-byte :pure)
(proclaim-function logtest (integer integer) gen-bool :pure)
(proclaim-function byte (unsigned-byte unsigned-byte) byte-specifier :pure)
(proclaim-function byte-size (byte-specifier) unsigned-byte :pure)
(proclaim-function byte-position (byte-specifier) unsigned-byte :pure)
(proclaim-function deposit-field (integer byte-specifier integer) integer :pure)
(proclaim-function dpb (integer byte-specifier integer) integer :pure)
(proclaim-function ldb (byte-specifier integer) unsigned-byte :pure)
(proclaim-function ldb-test (byte-specifier integer) gen-bool :pure)
(proclaim-function mask-field (byte-specifier integer) unsigned-byte :pure)
(proclaim-function decode-float (float) (values float integer float) :pure)
(proclaim-function scale-float (float integer) float :pure)
(proclaim-function float-radix (float) fixnum :pure)
(proclaim-function float-sign (float &optional float) float :pure)
(proclaim-function float-digits (float) fixnum :pure)
(proclaim-function float-precision (float) fixnum :pure)
(proclaim-function integer-decode-float (float)
                   (values float integer (member -1 1))
                   :pure)
(proclaim-function float (number &optional float) float :pure)
(proclaim-function floatp (t) gen-bool :pure)

;; Slot accessors:
;; (proclaim-function arithmetic-error-operands (condition) t)
;; (proclaim-function arithmetic-error-operation (condition) t)

;; ECL extensions
(proclaim-function si:bit-array-op (t t t t) t)


;;;
;;; 13. CHARACTERS
;;;

(proclaim-function char= (character &rest character) gen-bool :pure)
(proclaim-function char/= (character &rest character) gen-bool :pure)
(proclaim-function char< (character &rest character) gen-bool :pure)
(proclaim-function char> (character &rest character) gen-bool :pure)
(proclaim-function char<= (character &rest character) gen-bool :pure)
(proclaim-function char>= (character &rest character) gen-bool :pure)
(proclaim-function char-equal (character &rest character) gen-bool :pure)
(proclaim-function char-not-equal (character &rest character) gen-bool :pure)
(proclaim-function char-lessp (character &rest character) gen-bool :pure)
(proclaim-function char-greaterp (character &rest character) gen-bool :pure)
(proclaim-function char-not-greaterp (character &rest character) gen-bool :pure)
(proclaim-function char-not-lessp (character &rest character) gen-bool :pure)
(proclaim-function character (character-designator) character)
(proclaim-function characterp (t) gen-bool :pure)
(proclaim-function alpha-char-p (character) gen-bool :pure)
(proclaim-function alphanumericp (character) gen-bool :pure)
(proclaim-function digit-char (digit-weight &optional radix) character :pure)
(proclaim-function digit-char-p (character &optional radix)
                   (or digit-weight null)
                   :pure)
(proclaim-function graphic-char-p (character) gen-bool :pure)
(proclaim-function standard-char-p (character) gen-bool :pure)
(proclaim-function char-upcase (character) character :pure)
(proclaim-function char-downcase (character) character :pure)
(proclaim-function upper-case-p (character) gen-bool :pure)
(proclaim-function lower-case-p (character) gen-bool :pure)
(proclaim-function both-case-p (character) gen-bool :pure)
(proclaim-function char-code (character) character-code :pure)
(proclaim-function char-int (character) character-code :pure)
(proclaim-function code-char (character-code) (or character null) :pure)
(proclaim-function char-name (character) (or string null) :pure)
(proclaim-function name-char (string-designator) (or character null) :pure)

;; ECL extensions
(proclaim-function si:base-char-p (t) gen-bool :predicate)


;;;
;;; 14. CONSES
;;;

(proclaim-function cons (t t) cons :no-side-effects)
(proclaim-function consp (t) gen-bool :pure)
(proclaim-function atom (t) gen-bool :pure)
(proclaim-function rplaca (cons t) cons)
(proclaim-function rplacd (cons t) cons)
(proclaim-function car (list) t :reader)
(proclaim-function cdr (list) t :reader)
(proclaim-function caar (list) t :reader)
(proclaim-function cadr (list) t :reader)
(proclaim-function cdar (list) t :reader)
(proclaim-function cddr (list) t :reader)
(proclaim-function caaar (list) t :reader)
(proclaim-function caadr (list) t :reader)
(proclaim-function cadar (list) t :reader)
(proclaim-function caddr (list) t :reader)
(proclaim-function cdaar (list) t :reader)
(proclaim-function cdadr (list) t :reader)
(proclaim-function cddar (list) t :reader)
(proclaim-function cdddr (list) t :reader)
(proclaim-function caaaar (list) t :reader)
(proclaim-function caaadr (list) t :reader)
(proclaim-function caadar (list) t :reader)
(proclaim-function caaddr (list) t :reader)
(proclaim-function cadaar (list) t :reader)
(proclaim-function cadadr (list) t :reader)
(proclaim-function caddar (list) t :reader)
(proclaim-function cadddr (list) t :reader)
(proclaim-function cdaaar (list) t :reader)
(proclaim-function cdaadr (list) t :reader)
(proclaim-function cdadar (list) t :reader)
(proclaim-function cdaddr (list) t :reader)
(proclaim-function cddaar (list) t :reader)
(proclaim-function cddadr (list) t :reader)
(proclaim-function cdddar (list) t :reader)
(proclaim-function cddddr (list) t :reader)
(proclaim-function copy-tree (tree) tree :no-side-effects)
(proclaim-function sublis (association-list tree &key) tree :no-side-effects)
(proclaim-function nsublis (association-list tree &key) tree)
(proclaim-function subst (t t tree &key) tree :no-side-effects)
(proclaim-function subst-if (t function-designator tree &key) tree)
(proclaim-function subst-if-not (t function-designator tree &key) tree)
(proclaim-function nsubst (t t tree &key) tree)
(proclaim-function nsubst-if (t function-designator tree &key) tree)
(proclaim-function nsubst-if-not (t function-designator tree &key) tree)
(proclaim-function tree-equal (tree tree &key) gen-bool :predicate)
(proclaim-function copy-list (list) list :no-side-effects)
(proclaim-function list (&rest t) list :no-side-effects)
(proclaim-function list* (&rest t) t :no-side-effects)
(proclaim-function list-length (list) (or null si::index) :no-side-effects)
(proclaim-function listp (t) gen-bool :pure)
(proclaim-function make-list (si::index &key) list)
(proclaim-function first (list) t :reader)
(proclaim-function second (list) t :reader)
(proclaim-function third (list) t :reader)
(proclaim-function fourth (list) t :reader)
(proclaim-function fifth (list) t :reader)
(proclaim-function sixth (list) t :reader)
(proclaim-function seventh (list) t :reader)
(proclaim-function eighth (list) t :reader)
(proclaim-function ninth (list) t :reader)
(proclaim-function tenth (list) t :reader)
(proclaim-function nth (unsigned-byte list) t :reader)
(proclaim-function endp (list) gen-bool :predicate)
(proclaim-function null (t) gen-bool :predicate)
(proclaim-function nconc (*) t)
(proclaim-function append (*) t :no-side-effects)
(proclaim-function revappend (list t) t :no-side-effects)
(proclaim-function nreconc (list t) t)
(proclaim-function butlast (list &optional unsigned-byte) list :no-side-effects)
(proclaim-function nbutlast (list &optional unsigned-byte) list :no-side-effects)
(proclaim-function last (list &optional unsigned-byte) list :reader)
(proclaim-function ldiff (list t) list :no-side-effects)
(proclaim-function tailp (t list) gen-bool :reader)
(proclaim-function nthcdr (fixnum list) t :no-side-effects)
(proclaim-function rest (list) t :no-side-effects)
(proclaim-function member (t list &key) list :no-side-effects)
(proclaim-function member-if (function-designator list &key) list)
(proclaim-function member-if-not (function-designator list &key) list)
(proclaim-function mapc (function-designator list &rest list) list)
(proclaim-function mapcar (function-designator list &rest list) list)
(proclaim-function mapcan (function-designator list &rest list) list)
(proclaim-function mapl (function-designator list &rest list) list)
(proclaim-function maplist (function-designator list &rest list) list)
(proclaim-function mapcon (function-designator list &rest list) list)
(proclaim-function acons (t t association-list) association-list :no-side-effects)
(proclaim-function assoc (t association-list &key) t :no-side-effects)
(proclaim-function assoc-if (function-designator association-list &key) t)
(proclaim-function assoc-if-not (function-designator association-list &key) t)
(proclaim-function copy-alist (association-list) association-list :no-side-effects)
(proclaim-function pairlis (list list &optional association-list)
                   association-list :no-side-effects)
(proclaim-function rassoc (t association-list &key) t :no-side-effects)
(proclaim-function rassoc-if (function-designator association-list &key) t)
(proclaim-function rassoc-if-not (function-designator association-list &key) t)
(proclaim-function get-properties (list list) (values t t list) :no-side-effects)
(proclaim-function getf (list t &optional t) t :no-side-effects)
(proclaim-function intersection (list list &key) list :no-side-effects)
(proclaim-function nintersection (list list &key) list)
(proclaim-function adjoin (t list &key) list :no-side-effects)
(proclaim-function set-difference (list list &key) list :no-side-effects)
(proclaim-function nset-difference (list list &key) list)
(proclaim-function set-exclusive-or (list list &key) list :no-side-effects)
(proclaim-function nset-exclusive-or (list list &key) list)
(proclaim-function subsetp (list list &key) gen-bool :predicate)
(proclaim-function union (list list &key) list :no-side-effects)
(proclaim-function nunion (list list &key) list)

;; ECL extensions
(proclaim-function member1 (t list t t t) t)
(proclaim-function si:memq (t list) t)


;;;
;;; 15. ARRAYS
;;;

(proclaim-function make-array ((or si::index list) &key) array)
(proclaim-function adjust-array (array (or si::index list) &key) array)
(proclaim-function adjustable-array-p (array) gen-bool :pure)
(proclaim-function aref (array &rest si::index) t :reader)
(proclaim-function array-dimension (array (integer 0 #.(1- array-rank-limit)))
                   si::index :reader)
(proclaim-function array-dimensions (array) list :reader)
(proclaim-function array-element-type (array) type-specifier :pure)
(proclaim-function array-has-fill-pointer-p (array) gen-bool :pure)
(proclaim-function array-displacement (array) (values (or array null) si::index)
                   :reader)
(proclaim-function array-in-bounds-p (array &rest si::index) gen-bool
                   :no-side-effects)
(proclaim-function array-rank (array) (integer 0 #.(1- array-rank-limit))
                   :reader)
(proclaim-function array-row-major-index (array &rest si::index) si::index
                   :no-side-effects)
(proclaim-function array-total-size (array) si::index :reader)
(proclaim-function arrayp (t) gen-bool :pure)
(proclaim-function fill-pointer (vector) si::index :reader)
(proclaim-function row-major-aref (array si::index) t :reader)
(proclaim-function upgraded-array-element-type
                   (type-specifier &optional environment)
                   type-specifier :no-side-effects)
(proclaim-function simple-vector-p (t) gen-bool :pure)
(proclaim-function svref (simple-vector si::index) t :reader)
(proclaim-function vector (&rest t) vector :no-side-effects)
(proclaim-function vector-pop (vector) t)
(proclaim-function vector-push (t vector) (or si::index null))
(proclaim-function vector-push-extend (t vector &optional si::index) si::index)
(proclaim-function vectorp (t) gen-bool :pure)
(proclaim-function bit ((array bit) &rest si::index) bit :reader)
(proclaim-function sbit ((simple-array bit) &rest si::index)
                   bit :reader)
(proclaim-function bit-and (bit-array bit-array &optional
                            (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-andc1 (bit-array bit-array &optional
                              (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-andc2 (bit-array bit-array &optional
                              (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-eqv (bit-array bit-array &optional
                            (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-ior (bit-array bit-array &optional
                            (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-nand (bit-array bit-array &optional
                             (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-nor (bit-array bit-array &optional
                            (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-orc1 (bit-array bit-array &optional
                             (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-orc2 (bit-array bit-array &optional
                             (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-xor (bit-array bit-array &optional
                            (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-not (bit-array &optional (or bit-array (member t nil)))
                   bit-array)
(proclaim-function bit-vector-p (t) gen-bool :pure)
(proclaim-function simple-bit-vector-p (t) t :pure)

;; ECL extensions
(proclaim-function si:make-pure-array (*) array)
(proclaim-function si:make-vector (*) vector)
(proclaim-function si:aset (t array &rest si::index) t)
(proclaim-function si:row-major-aset (array si::index t) t)
(proclaim-function si:svset (simple-vector si::index t) t)
(proclaim-function si:fill-pointer-set (vector si::index) si::index)
(proclaim-function si:replace-array (array array) array)


;;;
;;; 16. STRINGS
;;;

(proclaim-function simple-string-p (t) gen-bool :pure)
(proclaim-function char (string si::index) character :reader)
(proclaim-function schar (simple-string si::index) character :reader)
(proclaim-function string (string-designator) string :no-side-effects)
(proclaim-function string-upcase (string-designator &key)
                   string :no-side-effects)
(proclaim-function string-downcase (string-designator &key)
                   string :no-side-effects)
(proclaim-function string-capitalize (string-designator &key)
                   string :no-side-effects)
(proclaim-function nstring-upcase (string &key) string)
(proclaim-function nstring-downcase (string &key) string)
(proclaim-function nstring-capitalize (string &key) string)
(proclaim-function string-trim (sequence string-designator)
                   string :no-side-effects)
(proclaim-function string-left-trim (sequence string-designator)
                   string :no-side-effects)
(proclaim-function string-right-trim (sequence string-designator)
                   string :no-side-effects)
(proclaim-function string= (string-designator string-designator &key)
                   gen-bool :no-side-effects)
(proclaim-function string/= (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string< (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string> (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string<= (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string>= (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string-equal (string-designator string-designator &key)
                   gen-bool :no-side-effects)
(proclaim-function string-not-equal (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string-lessp (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string-greaterp (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string-not-lessp (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function string-not-greaterp (string-designator string-designator &key)
                   (or si::index null) :no-side-effects)
(proclaim-function stringp (t) gen-bool :predicate)
(proclaim-function make-string (si::index &key) string :no-side-effects)

;; ECL extensions:
(proclaim-function si:base-string-p (t) gen-bool :predicate)
(proclaim-function si:char-set (string si::index character) character)
(proclaim-function si:schar-set (string si::index character) character)
(proclaim-function si:base-string-concatenate (base-string) base-string)


;;;
;;; 17. SEQUENCES
;;;

(proclaim-function copy-seq (sequence) sequence :no-side-effects)
(proclaim-function elt (sequence si::index) t :no-side-effects)
(proclaim-function fill (sequence t &key) sequence)
(proclaim-function make-sequence (type-specifier si::index &key) sequence)
(proclaim-function subseq (sequence si::index &optional (or si::index null))
                   sequence)
(proclaim-function map (type-specifier function-designator sequence &rest sequence)
                   sequence)
(proclaim-function map-into (sequence function-designator sequence &rest sequence)
                   sequence)
(proclaim-function reduce (function-designator sequence &key) t)
(proclaim-function count (t sequence &key) si::index :no-side-effects)
(proclaim-function count-if (function-designator sequence &key) si::index)
(proclaim-function count-if-not (function-designator sequence &key) si::index)
(proclaim-function length (sequence) si::index :no-side-effects)
(proclaim-function reverse (sequence) sequence :no-side-effects)
(proclaim-function nreverse (sequence) sequence)
(proclaim-function sort (sequence function-designator &key) sequence)
(proclaim-function stable-sort (sequence function-designator &key) sequence)
(proclaim-function find (t sequence &key) t :no-side-effects)
(proclaim-function find-if (function-designator sequence &key) t)
(proclaim-function find-if-not (function-designator sequence &key) t)
(proclaim-function position (t sequence &key) (or null si::index) :no-side-effects)
(proclaim-function position-if (function-designator sequence &key)
                   (or null si::index))
(proclaim-function position-if-not (function-designator sequence &key)
                   (or null si::index))
(proclaim-function search (sequence sequence &key)
                   (or null si::index) :no-side-effects)
(proclaim-function mismatch (sequence sequence &key)
                   (or null si::index) :no-side-effects)
(proclaim-function replace (sequence sequence &key) sequence)
(proclaim-function substitute (t t sequence &key) sequence :no-side-effects)
(proclaim-function substitute-if (t function-designator sequence &key) sequence)
(proclaim-function substitute-if-not (t function-designator sequence &key) sequence)
(proclaim-function nsubstitute (t t sequence &key) sequence)
(proclaim-function nsubstitute-if (t function-designator sequence &key) sequence)
(proclaim-function nsubstitute-if-not (t function-designator sequence &key) sequence)
(proclaim-function concatenate (type-specifier &rest sequence) sequence
                   :no-side-effects)
(proclaim-function merge (type-specifier sequence sequence function-designator &key)
                   sequence)
(proclaim-function remove (t sequence &key) sequence :no-side-effects)
(proclaim-function remove-if (function-designator sequence &key) sequence)
(proclaim-function remove-if-not (function-designator sequence &key) sequence)
(proclaim-function delete (t sequence &key) sequence)
(proclaim-function delete-if (function-designator sequence &key) sequence)
(proclaim-function delete-if-not (function-designator sequence &key) sequence)
(proclaim-function remove-duplicates (sequence &key) sequence :no-side-effects)
(proclaim-function delete-duplicates (sequence &key) sequence)

;; ECL extensions:
(proclaim-function si:elt-set (sequence si::index t) t)
(proclaim-function si::make-seq-iterator (t *) t :no-side-effects)
(proclaim-function si::seq-iterator-ref (t t) t :reader)
(proclaim-function si::seq-iterator-set (t t t) t :no-sp-change)
(proclaim-function si::seq-iterator-next (t t) t :reader)


;;;
;;; 18. HASH TABLES
;;;

(proclaim-function make-hash-table (&key) hash-table :no-side-effects)
(proclaim-function hash-table-p (t) gen-bool :pure)
(proclaim-function hash-table-count (hash-table) si::index :reader)
(proclaim-function hash-table-rehash-size (hash-table)
                   (or (integer 1 *) (float (1.0) *))
                   :reader)
(proclaim-function hash-table-rehash-threshold (hash-table)
                   (float (1.0) *)
                   :reader)
(proclaim-function hash-table-size (hash-table) si::index :reader)
(proclaim-function hash-table-test (hash-table) function-designator :reader)
(proclaim-function gethash (t hash-table &key) (values t gen-bool) :reader)
(proclaim-function remhash (t hash-table) gen-bool)
(proclaim-function maphash (function-designator hash-table) null)
(proclaim-function clrhash (hash-table) hash-table)
(proclaim-function sxhash (t) (integer 0 #.most-positive-fixnum) :no-side-effects)

;; ECL extensions
(proclaim-function si:hash-set (t hash-table t) t)


;;;
;;; 19. FILENAMES
;;;

(proclaim-function pathname (pathname-designator) pathname)
(proclaim-function make-pathname (&key) pathname)
(proclaim-function pathnamep (t) gen-bool :pure)
(proclaim-function pathname-host (pathname) pathname-host :reader)
(proclaim-function pathname-device (pathname) pathname-device :reader)
(proclaim-function pathname-directory (pathname) pathname-directory :reader)
(proclaim-function pathname-name (pathname) pathname-name :reader)
(proclaim-function pathname-type (pathname) pathname-type :reader)
(proclaim-function pathname-version (pathname) pathname-version :reader)
(proclaim-function load-logical-pathname-translations (string) gen-bool)
(proclaim-function logical-pathname-translations (string) list)
(proclaim-function logical-pathname (pathname-designator) logical-pathname)
(proclaim-function namestring (pathname-designator) (or string null))
(proclaim-function file-namestring (pathname-designator) (or string null))
(proclaim-function directory-namestring (pathname-designator) (or string null))
(proclaim-function host-namestring (pathname-designator) (or string null))
(proclaim-function enough-namestring (pathname-designator
                                      &optional pathname-designator)
                   (or string null))
(proclaim-function parse-namestring (pathname-designator
                                     &optional pathname-host
                                     pathname-designator &key)
                   (values (or pathname null) (or si::index null)))
(proclaim-function wild-pathname-p (pathname-designator
                                    &optional (member :host :device :directory :name
                                                      :type :version nil))
                   gen-bool)
(proclaim-function pathname-match-p (pathname-designator pathname-designator)
                   gen-bool)
(proclaim-function translate-logical-pathname (pathname-designator &key) pathname)
(proclaim-function translate-pathname (pathname-designator pathname-designator
                                                           pathname-designator &key)
                   pathname)
(proclaim-function merge-pathnames (pathname-designator
                                    &optional pathname-designator
                                    pathname-version)
                   pathname)

;;;
;;; 20. FILES
;;;

(proclaim-function directory (pathname-designator &key) list)
(proclaim-function probe-file (pathname-designator) (or pathname null))
(proclaim-function ensure-directories-exist (pathname &key)
                   (values pathname gen-bool))
(proclaim-function truename (pathname-designator) pathname)
(proclaim-function file-author (pathname-designator) (or string null))
(proclaim-function file-write-date (pathname-designator) (or unsigned-byte null))
(proclaim-function rename-file (pathname-designator pathname-designator)
                   (values pathname pathname pathname))
(proclaim-function delete-file (pathname-designator) t)

;; Slot accessors:
;; (proclaim-function file-error-pathname (condition) pathname-designator)

;; ECL extensions
(proclaim-function ext:file-kind (pathname-designator gen-bool) symbol)
(proclaim-function ext:chdir (pathname-designator &optional gen-bool) pathname)
(proclaim-function ext:getcwd (&optional gen-bool) pathname)
(proclaim-function ext:mkdir (pathname-designator fixnum) string)
(proclaim-function ext:mkstemp (pathname-designator) (or null pathname))
(proclaim-function ext:rmdir (pathname-designator) null)
(proclaim-function ext:copy-file (pathname-designator pathname-designator) gen-bool)


;;;
;;; 21. STREAMS
;;;

(proclaim-function input-stream-p (stream) gen-bool :reader)
(proclaim-function output-stream-p (stream) gen-bool :reader)
(proclaim-function interactive-stream-p (stream) gen-bool :reader)
(proclaim-function open-stream-p (stream) gen-bool :reader)
(proclaim-function stream-element-type (stream) type-specifier :reader)
(proclaim-function streamp (t) gen-bool :pure)
(proclaim-function read-byte (stream &optional gen-bool t) t)
(proclaim-function write-byte (integer stream) integer)
(proclaim-function peek-char (&optional (or character boolean)
                                        stream-designator
                                        gen-bool
                                        t
                                        gen-bool)
                   t)
(proclaim-function read-char (&optional stream-designator gen-bool t gen-bool) t)
(proclaim-function read-char-no-hang (&optional stream-designator gen-bool t gen-bool) t)
(proclaim-function terpri (&optional stream-designator) null)
(proclaim-function fresh-line (&optional stream-designator) gen-bool)
(proclaim-function unread-char (character &optional stream-designator) null)
(proclaim-function write-char (character &optional stream-designator) character)
(proclaim-function read-line (&optional stream-designator gen-bool t gen-bool)
                   (values t gen-bool))
(proclaim-function write-string (string &optional stream-designator &key) string)
(proclaim-function write-line (string &optional stream-designator &key) string)
(proclaim-function read-sequence (sequence stream &key) si::index)
(proclaim-function write-sequence (sequence stream &key) sequence)
(proclaim-function file-length (stream) unsigned-byte)
(proclaim-function file-position (stream file-position-designator) gen-bool)
(proclaim-function file-string-length (stream (or string character))
                   (or unsigned-byte null))
(proclaim-function open (pathname-designator &key) (or stream null))
(proclaim-function stream-external-format (stream) external-file-format :reader)
(proclaim-function close (stream &key) t)
(proclaim-function listen (&optional stream-designator) gen-bool)
(proclaim-function clear-input (&optional stream-designator) null)
(proclaim-function finish-output (&optional stream-designator) null)
(proclaim-function force-output (&optional stream-designator) null)
(proclaim-function clear-output (&optional stream-designator) null)
(proclaim-function y-or-n-p (&optional format-control &rest t) gen-bool)
(proclaim-function yes-or-no-p (&optional format-control &rest t) gen-bool)
(proclaim-function make-synonym-stream (symbol) synonym-stream)
(proclaim-function synonym-stream-symbol (synonym-stream) symbol :reader)
(proclaim-function broadcast-stream-streams (broadcast-stream) list :reader)
(proclaim-function make-broadcast-stream (&rest stream) broadcast-stream)
(proclaim-function make-two-way-stream (stream stream) two-way-stream)
(proclaim-function two-way-stream-input-stream (two-way-stream)
                   stream :reader)
(proclaim-function two-way-stream-output-stream (two-way-stream)
                   stream :reader)
(proclaim-function echo-stream-output-stream (echo-stream) stream :reader)
(proclaim-function echo-stream-input-stream (echo-stream) stream :reader)
(proclaim-function make-echo-stream (stream stream) echo-stream)
(proclaim-function concatenated-stream-streams (concatenated-stream)
                   list :reader)
(proclaim-function make-concatenated-stream (&rest stream) concatenated-stream)
(proclaim-function get-output-stream-string (string-stream)
                   string :reader)
(proclaim-function make-string-input-stream (string &optional
                                                    (or si::index null)
                                                    (or si::index null))
                   string-stream)
(proclaim-function make-string-output-stream (&key) string-stream)

;; Slot accessors:
;; (proclaim-function stream-error-stream (condition) stream)

;; ECL extensions:
(proclaim-function si:make-string-output-stream-from-string (string) string-stream)
(proclaim-function si:open-client-stream (t unsigned-byte) stream)
(proclaim-function si:open-server-stream (unsigned-byte) stream)
(proclaim-function si:open-unix-socket-stream (base-string) stream)
(proclaim-function si:lookup-host-entry (t) (values (or null string) list list))


;;;
;;; 22. PRINT
;;;

(proclaim-function copy-pprint-dispatch (&optional (or si::pprint-dispatch-table null))
                   si::pprint-dispatch-table)
(proclaim-function pprint-dispatch (t &optional (or si::pprint-dispatch-table null))
                   (values function-designator gen-bool))
(proclaim-function pprint-fill (stream-designator t &optional gen-bool gen-bool)
                   null)
(proclaim-function pprint-linear (stream-designator t &optional gen-bool gen-bool)
                   null)
(proclaim-function pprint-tabular (stream-designator t &optional gen-bool gen-bool
                                   unsigned-byte)
                   null)
(proclaim-function pprint-indent ((member :block :current) real
                                  &optional stream-designator)
                   null)
(proclaim-function pprint-newline ((member :linear :fill :miser :mandatory)
                                   &optional stream-designator)
                   null)
(proclaim-function pprint-tab ((member :line :section :line-relative :section-relative)
                               unsigned-byte unsigned-byte &optional stream-designator)
                   null)
(proclaim-function set-pprint-dispatch (type-specifier function-designator
                                        &optional real si::pprint-dispatch-table)
                   null)
(proclaim-function write (t &key) t)
(proclaim-function prin1 (t &optional stream-designator) t)
(proclaim-function princ (t &optional stream-designator) t)
(proclaim-function print (t &optional stream-designator) t)
(proclaim-function pprint (t &optional stream-designator) (values))
(proclaim-function write-to-string (t &key) string)
(proclaim-function prin1-to-string (t) string)
(proclaim-function princ-to-string (t) string)
(proclaim-function format ((or stream-designator t) format-control &rest t)
                   (or null string))

;; Slot accessor:
;; (proclaim-function print-not-readable-object (condition) t)


;;;
;;; 23. READER
;;;

(proclaim-function copy-readtable (&optional readtable-designator (or readtable null))
                   readtable)
(proclaim-function make-dispatch-macro-character
                   (character &optional gen-bool readtable)
                   (member t))
(proclaim-function read (&optional stream-designator gen-bool t gen-bool) t)
(proclaim-function read-preserving-whitespace
                   (&optional stream-designator gen-bool t gen-bool) t)
(proclaim-function read-delimited-list (character &optional stream-designator gen-bool)
                   list)
(proclaim-function read-from-string (string &optional gen-bool t &key)
                   (values t si::index))
(proclaim-function readtable-case (readtable)
                   (member :upcase :downcase :preserve :invert)
                   :reader)
(proclaim-function readtablep (t) gen-bool :pure)
(proclaim-function get-dispatch-macro-character
                   (character character &optional readtable-designator)
                   (or function-designator null)
                   :reader)
(proclaim-function set-dispatch-macro-character
                   (character character function-designator
                              &optional readtable-designator)
                   (member t))
(proclaim-function get-macro-character
                   (character &optional readtable-designator)
                   (values (or function-designator null) gen-bool)
                   :reader)
(proclaim-function set-macro-character
                   (character function-designator
                              &optional gen-bool readtable-designator)
                   (member t))
(proclaim-function set-syntax-from-char
                   (character character &optional readtable readtable-designator)
                   (member t))

;; ECL extensions:
(proclaim-function si:string-to-object (string &optional t) t)
(proclaim-function si:standard-readtable (t) readtable)


;;;
;;; 24. SYSTEM CONSTRUCTION
;;;

(proclaim-function compile-file (pathname-designator &key)
                   (values (or pathname null) gen-bool gen-bool))
(proclaim-function compile-file-pathname (pathname-designator &key)
                   pathname)
(proclaim-function load ((or stream pathname-designator) &key) gen-bool)
(proclaim-function provide (string-designator) t)
(proclaim-function require (string-designatior &optional list) t)


;;;
;;; 25. ENVIRONMENT
;;;

(proclaim-function decode-universal-time (universal-time &optional time-zone)
                   (values (integer 0 59)
                           (integer 0 59)
                           (integer 0 23)
                           (integer 1 31)
                           (integer 1 12)
                           unsigned-byte
                           (integer 0 6)
                           gen-bool
                           time-zone)
                   :pure)
(proclaim-function encode-universal-time ((integer 0 59)
                                          (integer 0 59)
                                          (integer 0 23)
                                          (integer 1 31)
                                          (integer 1 12)
                                          unsigned-byte
                                          &optional time-zone)
                   universal-time
                   :pure)
(proclaim-function get-universal-time () universal-time)
(proclaim-function get-decoded-time ()
                   (values (integer 0 59)
                           (integer 0 59)
                           (integer 0 23)
                           (integer 1 31)
                           (integer 1 12)
                           unsigned-byte
                           (integer 0 6)
                           gen-bool
                           time-zone))
(proclaim-function sleep ((real 0 *)) null)
(proclaim-function apropos (string-designator &optional (or null package-designator))
                   (values))
(proclaim-function apropos-list
                   (string-designator &optional (or null package-designator))
                   list)
(proclaim-function describe (t &optional stream-designator) (values))
(proclaim-function get-internal-real-time () unsigned-byte)
(proclaim-function get-internal-run-time () unsigned-byte)
(proclaim-function disassemble ((or function-designator list)) null)
(proclaim-function room (&optional (member t nil :default)) (values &rest t))
(proclaim-function ed (&optional (or null pathname string function-name))
                   (values &rest t))
(proclaim-function inspect (t) (values &rest t))
(proclaim-function dribble (&optional pathname-designator) (values &rest t))
(proclaim-function lisp-implementation-type () (or string null))
(proclaim-function lisp-implementation-version () (or string null))
(proclaim-function short-site-name () (or string null))
(proclaim-function long-site-name () (or string null))
(proclaim-function machine-instance () (or string null))
(proclaim-function machine-type () (or string null))
(proclaim-function machine-version () (or string null))
(proclaim-function software-type () (or string null))
(proclaim-function software-version () (or string null))
(proclaim-function user-homedir-pathname (&optional pathname-host)
                   (or pathname null))

;; ECL extensions

(proclaim-function si::room-report () (values t t t t t t t t))
(proclaim-function si::reset-gbc-count () t)
(proclaim-function ext:gc (&optional gen-bool) t)
(proclaim-function ext:quit (&optional fixnum) t)
(proclaim-function ext:argc () si::index)
(proclaim-function ext:argv () list)
(proclaim-function ext:getenv (string) (or null string))
(proclaim-function ext:system (string) fixnum)
(proclaim-function ext:getpid () fixnum)
(proclaim-function ext:make-pipe () (or two-way-stream null))
(proclaim-function ext:run-program (string list &key)
                   (values (or null two-way-stream)
                           (or null integer)))

;;;
;;; A. FFI
;;;

(proclaim-function si:pointer (t) unsigned-byte)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INLINE EXPANSIONS
;;;

(def-inline aref :unsafe (t t t) t
 "@0;ecl_aref_unsafe(#0,fix(#1)*(#0)->array.dims[1]+fix(#2))")
(def-inline aref :unsafe ((array t) t t) t
 "@0;(#0)->array.self.t[fix(#1)*(#0)->array.dims[1]+fix(#2)]")
(def-inline aref :unsafe ((array bit) t t) :fixnum
 "@0;ecl_aref_bv(#0,fix(#1)*(#0)->array.dims[1]+fix(#2))")
(def-inline aref :unsafe ((array t) fixnum fixnum) t
 "@0;(#0)->array.self.t[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array bit) fixnum fixnum) :fixnum
 "@0;ecl_aref_bv(#0,(#1)*(#0)->array.dims[1]+#2)")
(def-inline aref :unsafe ((array base-char) fixnum fixnum) :char
 "@0;(#0)->base_string.self[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array double-float) fixnum fixnum) :double
 "@0;(#0)->array.self.df[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array single-float) fixnum fixnum) :float
 "@0;(#0)->array.self.sf[#1*(#0)->array.dims[1]+#2]")
(def-inline aref :unsafe ((array fixnum) fixnum fixnum) :fixnum
 "@0;(#0)->array.self.fix[#1*(#0)->array.dims[1]+#2]")

(def-inline aref :always (t t) t "ecl_aref1(#0,fixint(#1))")
(def-inline aref :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline aref :unsafe (t t) t "ecl_aref1(#0,fix(#1))")
(def-inline aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,fix(#1))")
(def-inline aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
#+unicode
(def-inline aref :unsafe ((array character) fixnum) :wchar
 "(#0)->string.self[#1]")
(def-inline aref :unsafe ((array base-char) fixnum) :char
 "(#0)->base_string.self[#1]")
(def-inline aref :unsafe ((array double-float) fixnum) :double
 "(#0)->array.self.df[#1]")
(def-inline aref :unsafe ((array single-float) fixnum) :float
 "(#0)->array.self.sf[#1]")
(def-inline aref :unsafe ((array fixnum) fixnum) :fixnum
 "(#0)->array.self.fix[#1]")

(def-inline si:aset :unsafe (t t t t) t
 "@0;ecl_aset_unsafe(#1,fix(#2)*(#1)->array.dims[1]+fix(#3),#0)")
(def-inline si:aset :unsafe (t t fixnum fixnum) t
 "@0;ecl_aset_unsafe(#1,(#2)*(#1)->array.dims[1]+(#3),#0)")
(def-inline si:aset :unsafe (t (array t) fixnum fixnum) t
 "@1;(#1)->array.self.t[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (t (array bit) fixnum fixnum) :fixnum
 "@0;ecl_aset_bv(#1,(#2)*(#1)->array.dims[1]+(#3),fix(#0))")
(def-inline si:aset :unsafe (base-char (array base-char) fixnum fixnum) :char
 "@1;(#1)->base_string.self[#2*(#1)->array.dims[1]+#3]= #0")
#+unicode
(def-inline si:aset :unsafe (character (array character) fixnum fixnum) :wchar
 "@1;(#1)->string.self[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (double-float (array double-float) fixnum fixnum)
 :double "@1;(#1)->array.self.df[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (single-float (array single-float) fixnum fixnum)
 :float "@1;(#1)->array.self.sf[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (fixnum (array fixnum) fixnum fixnum) :fixnum
 "@1;(#1)->array.self.fix[#2*(#1)->array.dims[1]+#3]= #0")
(def-inline si:aset :unsafe (fixnum (array bit) fixnum fixnum) :fixnum
 "@0;ecl_aset_bv(#1,(#2)*(#1)->array.dims[1]+(#3),#0)")
(def-inline si:aset :always (t t t) t "ecl_aset1(#1,fixint(#2),#0)")
(def-inline si:aset :always (t t fixnum) t "ecl_aset1(#1,#2,#0)")
(def-inline si:aset :unsafe (t t t) t "ecl_aset1(#1,fix(#2),#0)")
(def-inline si:aset :unsafe (t (array t) fixnum) t
 "(#1)->vector.self.t[#2]= #0")
(def-inline si:aset :unsafe (t (array bit) fixnum) :fixnum
 "ecl_aset_bv(#1,#2,fix(#0))")
(def-inline si:aset :unsafe (base-char (array base-char) fixnum) :char
 "(#1)->base_string.self[#2]= #0")
#+unicode
(def-inline si:aset :unsafe (character (array character) fixnum) :wchar
 "(#1)->string.self[#2]= #0")
(def-inline si:aset :unsafe (double-float (array double-float) fixnum) :double
 "(#1)->array.self.df[#2]= #0")
(def-inline si:aset :unsafe (single-float (array single-float) fixnum) :float
 "(#1)->array.self.sf[#2]= #0")
(def-inline si:aset :unsafe (fixnum (array fixnum) fixnum) :fixnum
 "(#1)->array.self.fix[#2]= #0")
(def-inline si:aset :unsafe (fixnum (array bit) fixnum) :fixnum
 "ecl_aset_bv(#1,#2,#0)")

(def-inline row-major-aref :always (t t) t "ecl_aref(#0,fixint(#1))")
(def-inline row-major-aref :always (t fixnum) t "ecl_aref(#0,#1)")
(def-inline row-major-aref :unsafe (t t) t "ecl_aref_unsafe(#0,fix(#1))")
(def-inline row-major-aref :unsafe (t fixnum) t "ecl_aref_unsafe(#0,#1)")
(def-inline row-major-aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,fix(#1))")
(def-inline row-major-aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
#+unicode
(def-inline row-major-aref :unsafe ((array character) fixnum) :wchar
 "(#0)->string.self[#1]")
(def-inline row-major-aref :unsafe ((array base-char) fixnum) :char
 "(#0)->base_string.self[#1]")
(def-inline row-major-aref :unsafe ((array double-float) fixnum) :double
 "(#0)->array.self.df[#1]")
(def-inline row-major-aref :unsafe ((array single-float) fixnum) :float
 "(#0)->array.self.sf[#1]")
(def-inline row-major-aref :unsafe ((array fixnum) fixnum) :fixnum
 "(#0)->array.self.fix[#1]")

(def-inline si:row-major-aset :always (t t t) t "ecl_aset(#0,fixint(#1),#2)")
(def-inline si:row-major-aset :always (t fixnum t) t "ecl_aset(#0,#1,#2)")
(def-inline si:row-major-aset :unsafe (t t t) t "ecl_aset_unsafe(#0,fix(#1),#2)")
(def-inline si:row-major-aset :unsafe (t fixnum t) t "ecl_aset_unsafe(#0,#1,#2)")
(def-inline si:row-major-aset :unsafe ((array t) fixnum t) t
 "(#0)->vector.self.t[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array bit) fixnum t) :fixnum
 "ecl_aset_bv(#0,#1,fix(#2))")
(def-inline si:row-major-aset :unsafe ((array bit) fixnum fixnum) :fixnum
 "ecl_aset_bv(#0,#1,#2)")
(def-inline si:row-major-aset :unsafe ((array base-char) fixnum base-char) :char
 "(#0)->base_string.self[#1]= #2")
#+unicode
(def-inline si:row-major-aset :unsafe ((array character) fixnum character) :wchar
 "(#0)->string.self[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array double-float) fixnum double-float) :double
 "(#0)->array.self.df[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array single-float) fixnum single-float) :float
 "(#0)->array.self.sf[#1]= #2")
(def-inline si:row-major-aset :unsafe ((array fixnum) fixnum fixnum) :fixnum
 "(#0)->array.self.fix[#1]= #2")

(def-inline array-rank :unsafe (array) :fixnum
 "(#0)->array.rank")

(def-inline array-dimension :always (t t) fixnum
 "ecl_array_dimension(#0,fixint(#1))")
(def-inline array-dimension :always (t fixnum) fixnum
 "ecl_array_dimension(#0,#1)")

(def-inline array-total-size :unsafe (t) :fixnum "((#0)->array.dim)")

(def-inline svref :always (t t) t "ecl_aref1(#0,fixint(#1))")
(def-inline svref :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline svref :unsafe (t t) t "(#0)->vector.self.t[fix(#1)]")
(def-inline svref :unsafe (t fixnum) t "(#0)->vector.self.t[#1]")

(def-inline si:svset :always (t t t) t "ecl_aset1(#0,fixint(#1),#2)")
(def-inline si:svset :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
(def-inline si:svset :unsafe (t t t) t "((#0)->vector.self.t[fix(#1)]=(#2))")
(def-inline si:svset :unsafe (t fixnum t) t "(#0)->vector.self.t[#1]= #2")

(def-inline fill-pointer :unsafe (t) :fixnum "((#0)->vector.fillp)")

(def-inline si:fill-pointer-set :unsafe (t fixnum) :fixnum
 "((#0)->vector.fillp)=(#1)")

;; file character.d

(def-inline standard-char-p :always (character) :bool "ecl_standard_char_p(#0)")

(def-inline graphic-char-p :always (character) :bool "ecl_graphic_char_p(#0)")

(def-inline alpha-char-p :always (character) :bool "ecl_alpha_char_p(#0)")

(def-inline upper-case-p :always (character) :bool "ecl_upper_case_p(#0)")

(def-inline lower-case-p :always (character) :bool "ecl_lower_case_p(#0)")

(def-inline both-case-p :always (character) :bool "ecl_both_case_p(#0)")

(def-inline alphanumericp :always (character) :bool "ecl_alphanumericp(#0)")

(def-inline char= :always (t t) :bool "ecl_char_code(#0)==ecl_char_code(#1)")
(def-inline char= :always (character character) :bool "(#0)==(#1)")

(def-inline char/= :always (t t) :bool "ecl_char_code(#0)!=ecl_char_code(#1)")
(def-inline char/= :always (character character) :bool "(#0)!=(#1)")

(def-inline char< :always (character character) :bool "(#0)<(#1)")

(def-inline char> :always (character character) :bool "(#0)>(#1)")

(def-inline char<= :always (character character) :bool "(#0)<=(#1)")

(def-inline char>= :always (character character) :bool "(#0)>=(#1)")

(def-inline char-code :always (character) :fixnum "#0")

(def-inline code-char :always (fixnum) :char "#0")

(def-inline char-upcase :always (base-char) :char "ecl_char_upcase(#0)")
(def-inline char-upcase :always (character) :wchar "ecl_char_upcase(#0)")

(def-inline char-downcase :always (base-char) :char "ecl_char_downcase(#0)")
(def-inline char-downcase :always (character) :wchar "ecl_char_downcase(#0)")

(def-inline char-int :always (character) :fixnum "#0")

;; file file.d

(def-inline input-stream-p :always (stream) :bool "ecl_input_stream_p(#0)")

(def-inline output-stream-p :always (stream) :bool "ecl_output_stream_p(#0)")

;; file list.d

(def-inline car :always (cons) t "CAR(#0)")
(def-inline car :unsafe (t) t "CAR(#0)")

(def-inline cdr :always (cons) t "CDR(#0)")
(def-inline cdr :unsafe (t) t "CDR(#0)")

(def-inline caar :always (cons) t "CAAR(#0)")
(def-inline caar :unsafe (t) t "CAAR(#0)")

(def-inline cadr :always (cons) t "CADR(#0)")
(def-inline cadr :unsafe (t) t "CADR(#0)")

(def-inline cdar :always (cons) t "CDAR(#0)")
(def-inline cdar :unsafe (t) t "CDAR(#0)")

(def-inline cddr :always (cons) t "CDDR(#0)")
(def-inline cddr :unsafe (t) t "CDDR(#0)")

(def-inline caaar :always (cons) t "CAAAR(#0)")
(def-inline caaar :unsafe (t) t "CAAAR(#0)")

(def-inline caadr :always (cons) t "CAADR(#0)")
(def-inline caadr :unsafe (t) t "CAADR(#0)")

(def-inline cadar :always (cons) t "CADAR(#0)")
(def-inline cadar :unsafe (t) t "CADAR(#0)")

(def-inline caddr :always (cons) t "CADDR(#0)")
(def-inline caddr :unsafe (t) t "CADDR(#0)")

(def-inline cdaar :always (cons) t "CDAAR(#0)")
(def-inline cdaar :unsafe (t) t "CDAAR(#0)")

(def-inline cdadr :always (cons) t "CDADR(#0)")
(def-inline cdadr :unsafe (t) t "CDADR(#0)")

(def-inline cddar :always (cons) t "CDDAR(#0)")
(def-inline cddar :unsafe (t) t "CDDAR(#0)")

(def-inline cdddr :always (cons) t "CDDDR(#0)")
(def-inline cdddr :unsafe (t) t "CDDDR(#0)")

(def-inline caaaar :always (cons) t "CAAAAR(#0)")
(def-inline caaaar :unsafe (t) t "CAAAAR(#0)")

(def-inline caaadr :always (cons) t "CAAADR(#0)")
(def-inline caaadr :unsafe (t) t "CAAADR(#0)")

(def-inline caadar :always (cons) t "CAADAR(#0)")
(def-inline caadar :unsafe (t) t "CAADAR(#0)")

(def-inline caaddr :always (cons) t "CAADDR(#0)")
(def-inline caaddr :unsafe (t) t "CAADDR(#0)")

(def-inline cadaar :always (cons) t "CADAAR(#0)")
(def-inline cadaar :unsafe (t) t "CADAAR(#0)")

(def-inline cadadr :always (cons) t "CADADR(#0)")
(def-inline cadadr :unsafe (t) t "CADADR(#0)")

(def-inline caddar :always (cons) t "CADDAR(#0)")
(def-inline caddar :unsafe (t) t "CADDAR(#0)")

(def-inline cadddr :always (cons) t "CADDDR(#0)")
(def-inline cadddr :unsafe (t) t "CADDDR(#0)")

(def-inline cdaaar :always (cons) t "CDAAAR(#0)")
(def-inline cdaaar :unsafe (t) t "CDAAAR(#0)")

(def-inline cdaadr :always (cons) t "CDAADR(#0)")
(def-inline cdaadr :unsafe (t) t "CDAADR(#0)")

(def-inline cdadar :always (cons) t "CDADAR(#0)")
(def-inline cdadar :unsafe (t) t "CDADAR(#0)")

(def-inline cdaddr :always (cons) t "CDADDR(#0)")
(def-inline cdaddr :unsafe (t) t "CDADDR(#0)")

(def-inline cddaar :always (cons) t "CDDAAR(#0)")
(def-inline cddaar :unsafe (t) t "CDDAAR(#0)")

(def-inline cddadr :always (cons) t "CDDADR(#0)")
(def-inline cddadr :unsafe (t) t "CDDADR(#0)")

(def-inline cdddar :always (cons) t "CDDDAR(#0)")
(def-inline cdddar :unsafe (t) t "CDDDAR(#0)")

(def-inline cddddr :always (cons) t "CDDDDR(#0)")
(def-inline cddddr :unsafe (t) t "CDDDDR(#0)")

(def-inline cons :always (t t) t "CONS(#0,#1)")

(def-inline endp :safe (t) :bool "ecl_endp(#0)")
(def-inline endp :unsafe (t) :bool "#0==Cnil")

(def-inline nth :always (t t) t "ecl_nth(fixint(#0),#1)")
(def-inline nth :always (fixnum t) t "ecl_nth(#0,#1)")
(def-inline nth :unsafe (t t) t "ecl_nth(fix(#0),#1)")
(def-inline nth :unsafe (fixnum t) t "ecl_nth(#0,#1)")

(def-inline first :always (cons) t "ECL_CONS_CAR(#0)")
(def-inline first :unsafe (t) t "CAR(#0)")

(def-inline second :always (cons) t "CADR(#0)")
(def-inline second :unsafe (t) t "CADR(#0)")

(def-inline third :always (cons) t "CADDR(#0)")
(def-inline third :unsafe (t) t "CADDR(#0)")

(def-inline fourth :always (cons) t "CADDDR(#0)")
(def-inline fourth :unsafe (t) t "CADDDR(#0)")

(def-inline rest :always (cons) t "ECL_CONS_CDR(#0)")
(def-inline rest :unsafe (t) t "CDR(#0)")

(def-inline nthcdr :always (t t) t "ecl_nthcdr(fixint(#0),#1)")
(def-inline nthcdr :always (fixnum t) t "ecl_nthcdr(#0,#1)")
(def-inline nthcdr :unsafe (t t) t "ecl_nthcdr(fix(#0),#1)")
(def-inline nthcdr :unsafe (fixnum t) t "ecl_nthcdr(#0,#1)")

(def-inline last :always (t) t "ecl_last(#0,1)")
(def-inline list :always nil t "Cnil")
(def-inline list :always (t) t "ecl_list1(#0)")

(def-inline list* :always (t) t "#0")
(def-inline list* :always (t t) t "CONS(#0,#1)")

(def-inline append :always (t t) t "ecl_append(#0,#1)")

(def-inline nconc :always (t t) t "ecl_nconc(#0,#1)")

(def-inline butlast :always (t) t "ecl_butlast(#0,1)")

(def-inline nbutlast :always (t) t "ecl_nbutlast(#0,1)")

;; file num_arith.d

(def-inline + :always (t t) t "ecl_plus(#0,#1)")
(def-inline + :always (fixnum-float fixnum-float) :double
 "(double)(#0)+(double)(#1)" :exact-return-type t)
(def-inline + :always (fixnum-float fixnum-float) :float
 "(float)(#0)+(float)(#1)" :exact-return-type t)
(def-inline + :always (fixnum fixnum) :fixnum "(#0)+(#1)" :exact-return-type t)

(def-inline - :always (t) t "ecl_negate(#0)")
(def-inline - :always (t t) t "ecl_minus(#0,#1)")
(def-inline - :always (fixnum-float fixnum-float) :double
 "(double)(#0)-(double)(#1)" :exact-return-type t)
(def-inline - :always (fixnum-float fixnum-float) :float
 "(float)(#0)-(float)(#1)" :exact-return-type t)
(def-inline - :always (fixnum fixnum) :fixnum "(#0)-(#1)" :exact-return-type t)
(def-inline - :always (fixnum-float) :double "-(double)(#0)" :exact-return-type t)
(def-inline - :always (fixnum-float) :float "-(float)(#0)" :exact-return-type t)
(def-inline - :always (fixnum) :fixnum "-(#0)" :exact-return-type t)

(def-inline * :always (t t) t "ecl_times(#0,#1)")
(def-inline * :always (fixnum-float fixnum-float) :double
 "(double)(#0)*(double)(#1)" :exact-return-type t)
(def-inline * :always (fixnum-float fixnum-float) :float
 "(float)(#0)*(float)(#1)" :exact-return-type t)
(def-inline * :always (fixnum fixnum) t "fixnum_times(#0,#1)" :exact-return-type t)
(def-inline * :always (fixnum fixnum) :fixnum "(#0)*(#1)" :exact-return-type t)

(def-inline / :always (t t) t "ecl_divide(#0,#1)")
(def-inline / :always (fixnum-float fixnum-float) :double
 "(double)(#0)/(double)(#1)" :exact-return-type t)
(def-inline / :always (fixnum-float fixnum-float) :float
 "(float)(#0)/(float)(#1)" :exact-return-type t)
(def-inline / :always (fixnum fixnum) :fixnum "(#0)/(#1)" :exact-return-type t)

(def-inline 1+ :always (t) t "ecl_one_plus(#0)")
(def-inline 1+ :always (double-loat) :double "(double)(#0)+1")
(def-inline 1+ :always (single-float) :float "(float)(#0)+1")
(def-inline 1+ :always (fixnum) :fixnum "(#0)+1" :exact-return-type t)

(def-inline 1- :always (t) t "ecl_one_minus(#0)")
(def-inline 1- :always (double-float) :double "(double)(#0)-1")
(def-inline 1- :always (single-float) :float "(float)(#0)-1")
(def-inline 1- :always (fixnum) :fixnum "(#0)-1" :exact-return-type t)

;; file num_co.d

(def-inline float :always (t single-float) :float "ecl_to_float(#0)")
(def-inline float :always (t double-float) :double "ecl_to_double(#0)")
(def-inline float :always (fixnum-float) :double "((double)(#0))" :exact-return-type t)
(def-inline float :always (fixnum-float) :float "((float)(#0))" :exact-return-type t)

(def-inline numerator :unsafe (integer) integer "(#0)")
(def-inline numerator :unsafe (ratio) integer "(#0)->ratio.num")

(def-inline denominator :unsafe (integer) integer "MAKE_FIXNUM(1)")
(def-inline denominator :unsafe (ratio) integer "(#0)->ratio.den")

(def-inline floor :always (t) (values &rest t) "ecl_floor1(#0)")
(def-inline floor :always (t t) (values &rest t) "ecl_floor2(#0,#1)")
(def-inline floor :always (fixnum fixnum) :fixnum
 "@01;(#0>=0&&#1>0?(#0)/(#1):ecl_ifloor(#0,#1))")

(def-inline ceiling :always (t) (values &rest t) "ecl_ceiling1(#0)")
(def-inline ceiling :always (t t) (values &rest t) "ecl_ceiling2(#0,#1)")

(def-inline truncate :always (t) (values &rest t) "ecl_truncate1(#0)")
(def-inline truncate :always (t t) (values &rest t) "ecl_truncate2(#0,#1)")
(def-inline truncate :always (fixnum-float) :fixnum "(cl_fixnum)(#0)")

(def-inline round :always (t) (values &rest t) "ecl_round1(#0)")
(def-inline round :always (t t) (values &rest t) "ecl_round2(#0,#1)")

(def-inline mod :always (t t) t "(ecl_floor2(#0,#1),cl_env_copy->values[1])")
(def-inline mod :always (fixnum fixnum) :fixnum
 "@01;(#0>=0&&#1>0?(#0)%(#1):ecl_imod(#0,#1))")

(def-inline rem :always (t t) t "(ecl_truncate2(#0,#1),cl_env_copy->values[1])")
(def-inline rem :always (fixnum fixnum) :fixnum "(#0)%(#1)")

(def-inline = :always (t t) :bool "ecl_number_equalp(#0,#1)")
(def-inline = :always (fixnum-float fixnum-float) :bool "(#0)==(#1)")

(def-inline /= :always (t t) :bool "!ecl_number_equalp(#0,#1)")
(def-inline /= :always (fixnum-float fixnum-float) :bool "(#0)!=(#1)")

(def-inline < :always (t t) :bool "ecl_number_compare(#0,#1)<0")
(def-inline < :always (fixnum-float fixnum-float) :bool "(#0)<(#1)")
(def-inline < :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)<(#1) && (#1)<(#2))")

(def-inline > :always (t t) :bool "ecl_number_compare(#0,#1)>0")
(def-inline > :always (fixnum-float fixnum-float) :bool "(#0)>(#1)")
(def-inline > :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)>(#1) && (#1)>(#2))")

(def-inline <= :always (t t) :bool "ecl_number_compare(#0,#1)<=0")
(def-inline <= :always (fixnum-float fixnum-float) :bool "(#0)<=(#1)")
(def-inline <= :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)<=(#1) && (#1)<=(#2))")

(def-inline >= :always (t t) :bool "ecl_number_compare(#0,#1)>=0")
(def-inline >= :always (fixnum-float fixnum-float) :bool "(#0)>=(#1)")
(def-inline >= :always (fixnum-float fixnum-float fixnum-float) :bool
            "@012;((#0)>=(#1) && (#1)>=(#2))")

(def-inline max :always (t t) t "@01;(ecl_number_compare(#0,#1)>=0?#0:#1)")
(def-inline max :always (fixnum fixnum) :fixnum "@01;(#0)>=(#1)?#0:#1")

(def-inline min :always (t t) t "@01;(ecl_number_compare(#0,#1)<=0?#0:#1)")
(def-inline min :always (fixnum fixnum) :fixnum "@01;(#0)<=(#1)?#0:#1")

;; file num_log.d

(def-inline logand :always nil t "MAKE_FIXNUM(-1)")
(def-inline logand :always nil :fixnum "-1")
(def-inline logand :always (t t) t "ecl_boole(ECL_BOOLAND,(#0),(#1))")
(def-inline logand :always (fixnum fixnum) :fixnum "((#0) & (#1))")

(def-inline logandc1 :always (t t) t "ecl_boole(ECL_BOOLANDC1,(#0),(#1))")
(def-inline logandc1 :always (fixnum fixnum) :fixnum "(~(#0) & (#1))")

(def-inline logandc2 :always (t t) t "ecl_boole(ECL_BOOLANDC2,(#0),(#1))")
(def-inline logandc2 :always (fixnum fixnum) :fixnum "((#0) & ~(#1))")

(def-inline logeqv :always nil t "MAKE_FIXNUM(-1)")
(def-inline logeqv :always nil :fixnum "-1")
(def-inline logeqv :always (t t) t "ecl_boole(ECL_BOOLEQV,(#0),(#1))")
(def-inline logeqv :always (fixnum fixnum) :fixnum "(~( (#0) ^ (#1) ))")

(def-inline logior :always nil t "MAKE_FIXNUM(0)")
(def-inline logior :always nil :fixnum "0")
(def-inline logior :always (t t) t "ecl_boole(ECL_BOOLIOR,(#0),(#1))")
(def-inline logior :always (fixnum fixnum) :fixnum "((#0) | (#1))")

(def-inline lognand :always (t t) t "ecl_boole(ECL_BOOLNAND,(#0),(#1))")
(def-inline lognand :always (fixnum fixnum) :fixnum "(~( (#0) & (#1) ))")

(def-inline lognor :always (t t) t "ecl_boole(ECL_BOOLNOR,(#0),(#1))")
(def-inline lognor :always (fixnum fixnum) :fixnum "(~( (#0) | (#1) ))")

(def-inline lognot :always (t) t "ecl_boole(ECL_BOOLXOR,(#0),MAKE_FIXNUM(-1))")
(def-inline lognot :always (fixnum) :fixnum "(~(#0))")

(def-inline logorc1 :always (t t) t "ecl_boole(ECL_BOOLORC1,(#0),(#1))")
(def-inline logorc1 :always (fixnum fixnum) :fixnum "(~(#0) | (#1))")

(def-inline logorc2 :always (t t) t "ecl_boole(ECL_BOOLORC2,(#0),(#1))")
(def-inline logorc2 :always (fixnum fixnum) :fixnum "((#0) | ~(#1))")

(def-inline logxor :always nil t "MAKE_FIXNUM(0)")
(def-inline logxor :always nil :fixnum "0")
(def-inline logxor :always (t t) t "ecl_boole(ECL_BOOLXOR,(#0),(#1))")
(def-inline logxor :always (fixnum fixnum) :fixnum "((#0) ^ (#1))")

(def-inline boole :always (fixnum t t) t "ecl_boole((#0),(#1),(#2))")

(def-inline logbitp :always ((integer -29 29) fixnum) :bool "(#1 >> #0) & 1")

(def-inline zerop :always (t) :bool "ecl_zerop(#0)")
(def-inline zerop :always (fixnum-float) :bool "(#0)==0")

(def-inline plusp :always (t) :bool "ecl_plusp(#0)")
(def-inline plusp :always (fixnum-float) :bool "(#0)>0")

(def-inline minusp :always (t) :bool "ecl_minusp(#0)")
(def-inline minusp :always (fixnum-float) :bool "(#0)<0")

(def-inline oddp :always (t) :bool "ecl_oddp(#0)")
(def-inline oddp :always (fixnum fixnum) :bool "(#0) & 1")

(def-inline evenp :always (t) :bool "ecl_evenp(#0)")
(def-inline evenp :always (fixnum fixnum) :bool "~(#0) & 1")

(def-inline expt :always ((integer 2 2) (integer 0 29)) :fixnum "(1<<(#1))")
(def-inline expt :always ((integer 0 0) t) :fixnum "0")
(def-inline expt :always ((integer 1 1) t) :fixnum "1")

(def-inline log :always (fixnum-float) :double "log((double)(#0))" :exact-return-type t)
(def-inline log :always (fixnum-float) :float "(float)log((double)(#0))" :exact-return-type t)

(def-inline sqrt :always ((or (long-float 0.0 *) (double-float 0.0 *))) :double "sqrt((double)(#0))")
(def-inline sqrt :always ((or (single-float 0.0 *) (short-float 0.0 *))) :float "(float)sqrt((double)(#0))")

(def-inline sin :always (fixnum-float) :double "sin((double)(#0))" :exact-return-type t)
(def-inline sin :always (fixnum-float) :float "(float)sin((double)(#0))" :exact-return-type t)

(def-inline cos :always (fixnum-float) :double "cos((double)(#0))" :exact-return-type t)
(def-inline cos :always (fixnum-float) :float "(float)cos((double)(#0))" :exact-return-type t)

(def-inline tan :always (fixnum-float) :double "tan((double)(#0))" :exact-return-type t)
(def-inline tan :always (fixnum-float) :float "(float)tan((double)(#0))" :exact-return-type t)

(def-inline sin :always (fixnum-float) :double "sinh((double)(#0))" :exact-return-type t)
(def-inline sin :always (fixnum-float) :float "(float)sinh((double)(#0))" :exact-return-type t)

(def-inline cos :always (fixnum-float) :double "cosh((double)(#0))" :exact-return-type t)
(def-inline cos :always (fixnum-float) :float "(float)cosh((double)(#0))" :exact-return-type t)

(def-inline tan :always (fixnum-float) :double "tanh((double)(#0))" :exact-return-type t)
(def-inline tan :always (fixnum-float) :float "(float)tanh((double)(#0))" :exact-return-type t)

;; file pathname.d

(def-inline null :always (t) :bool "#0==Cnil")

(def-inline symbolp :always (t) :bool "SYMBOLP(#0)")

(def-inline atom :always (t) :bool "ATOM(#0)")

(def-inline consp :always (t) :bool "CONSP(#0)")

(def-inline listp :always (t) :bool "@0;LISTP(#0)")

(def-inline numberp :always (t) :bool "ecl_numberp(#0)")

(def-inline integerp :always (t) :bool
 "@0;type_of(#0)==t_fixnum||type_of(#0)==t_bignum")

(def-inline floatp :always (t) :bool "floatp(#0)")

(def-inline characterp :always (t) :bool "CHARACTERP(#0)")

(def-inline base-char-p :always (character) :bool "BASE_CHAR_P(#0)")

(def-inline stringp :always (t) :bool "ecl_stringp(#0)")

(def-inline base-string-p :always (t) :bool "type_of(#0)==t_base_string")

(def-inline bit-vector-p :always (t) :bool "(type_of(#0)==t_bitvector)")

(def-inline vectorp :always (t) :bool "@0;ECL_VECTORP(#0)")

(def-inline arrayp :always (t) :bool "@0;ECL_ARRAYP(#0)")

(def-inline eq :always (t t) :bool "(#0)==(#1)")
(def-inline eq :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline eql :always (t t) :bool "ecl_eql(#0,#1)")
(def-inline eql :always (character t) :bool "(CODE_CHAR(#0)==(#1))")
(def-inline eql :always (t character) :bool "((#0)==CODE_CHAR(#1))")
(def-inline eql :always (character character) :bool "(#0)==(#1)")
(def-inline eql :always ((not (or complex bignum ratio float)) t) :bool
 "(#0)==(#1)")
(def-inline eql :always (t (not (or complex bignum ratio float))) :bool
 "(#0)==(#1)")
(def-inline eql :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline equal :always (t t) :bool "ecl_equal(#0,#1)")
(def-inline equal :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline equalp :always (t t) :bool "ecl_equalp(#0,#1)")
(def-inline equalp :always (fixnum fixnum) :bool "(#0)==(#1)")

(def-inline not :always (t) :bool "(#0)==Cnil")

;; file print.d, read.d

(def-inline clear-output :always (stream) NULL "(ecl_clear_output(#0),Cnil)")

(def-inline finish-output :always (stream) NULL "(ecl_finish_output(#0),Cnil)")

(def-inline finish-output :always (stream) NULL "(ecl_force_output(#0),Cnil)")

(def-inline prin1 :always (t t) t "ecl_prin1(#0,#1)")
(def-inline prin1 :always (t) t "ecl_prin1(#0,Cnil)")

(def-inline princ :always (t t) t "ecl_princ(#0,#1)")
(def-inline princ :always (t) t "ecl_princ(#0,Cnil)")

(def-inline print :always (t t) t "ecl_print(#0,#1)")
(def-inline print :always (t) t "ecl_print(#0,Cnil)")

(def-inline terpri :always (t) t "ecl_terpri(#0)")
(def-inline terpri :always nil t "ecl_terpri(Cnil)")

(def-inline write-char :always (t) t "@0;(ecl_princ_char(ecl_char_code(#0),Cnil),(#0))")

(def-inline clear-input :always (stream) NULL "(ecl_clear_input(#0),Cnil)")

(def-inline copy-readtable :always (null null) t "standard_readtable")

(def-inline boundp :always (symbol) :bool "ECL_SYM_VAL(cl_env_copy,#0)!=OBJNULL")

;; file sequence.d

(def-inline elt :always (t t) t "ecl_elt(#0,fix(#1))")
(def-inline elt :always (t fixnum) t "ecl_elt(#0,#1)")
(def-inline elt :always (vector t) t "ecl_aref1(#0,fix(#1))")
(def-inline elt :always (vector fixnum) t "ecl_aref1(#0,#1)")

(def-inline elt :unsafe (t t) t "ecl_elt(#0,fix(#1))")
(def-inline elt :unsafe (t fixnum) t "ecl_elt(#0,#1)")
(def-inline elt :unsafe (vector t) t "ecl_aref_unsafe(#0,fix(#1))")
(def-inline elt :unsafe (vector fixnum) t "ecl_elt_unsafe(#0,#1)")
(def-inline aref :unsafe ((array bit) t) :fixnum "ecl_aref_bv(#0,fix(#1))")
(def-inline aref :unsafe ((array bit) fixnum) :fixnum "ecl_aref_bv(#0,#1)")
#+unicode
(def-inline aref :unsafe ((array character) fixnum) :wchar
 "(#0)->string.self[#1]")
(def-inline aref :unsafe ((array base-char) fixnum) :char
 "(#0)->base_string.self[#1]")
(def-inline aref :unsafe ((array double-float) fixnum) :double
 "(#0)->array.self.df[#1]")
(def-inline aref :unsafe ((array single-float) fixnum) :float
 "(#0)->array.self.sf[#1]")
(def-inline aref :unsafe ((array fixnum) fixnum) :fixnum
 "(#0)->array.self.fix[#1]")

(def-inline si:elt-set :always (t t t) t "ecl_elt_set(#0,fixint(#1),#2)")
(def-inline si:elt-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
(def-inline si:elt-set :always (vector t t) t "ecl_aset1(#0,fixint(#1),#2)")
(def-inline si:elt-set :always (vector fixnum t) t "ecl_aset1(#0,#1,#2)")

(def-inline si:elt-set :unsafe (t t t) t "ecl_elt_set(#0,fix(#1),#2)")
(def-inline si:elt-set :unsafe (vector t t) t "ecl_aset1_unsafe(#0,fixint(#1),#2)")
(def-inline si:elt-set :unsafe (vector fixnum t) t "ecl_aset1_unsafe(#0,#1,#2)")

(def-inline length :always (t) :fixnum "ecl_length(#0)")
(def-inline length :unsafe (array t) :fixnum "(#0)->vector.fillp")

;; file character.d

(def-inline char :always (t fixnum) t "ecl_aref1(#0,#1)")
(def-inline char :always (t fixnum) :wchar "ecl_char(#0,#1)")
#-unicode
(def-inline char :unsafe (t t) t "CODE_CHAR((#0)->base_string.self[fix(#1)])")
#-unicode
(def-inline char :unsafe (t fixnum) :char "(#0)->base_string.self[#1]")
(def-inline char :unsafe (base-string fixnum) :unsigned-char "(#0)->base_string.self[#1]")
#+unicode
(def-inline char :unsafe (ext:extended-string fixnum) :wchar "(#0)->string.self[#1]")

(def-inline si:char-set :always (t t t) t "si_char_set(#0,#1,#2)")
(def-inline si:char-set :always (t fixnum t) t "ecl_aset1(#0,#1,#2)")
(def-inline si:char-set :always (t fixnum character) :wchar "ecl_char_set(#0,#1,#2)")
#-unicode
(def-inline si:char-set :unsafe (t t t) t
 "@2;((#0)->base_string.self[fix(#1)]=ecl_char_code(#2),(#2))")
#-unicode
(def-inline si:char-set :unsafe (t fixnum character) :char
 "(#0)->base_string.self[#1]= #2")
(def-inline si:char-set :unsafe (base-string t t) t
 "@2;((#0)->base_string.self[fix(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:char-set :unsafe (base-string fixnum base-char) :char
 "(#0)->base_string.self[#1]= #2")
(def-inline si:char-set :unsafe (ext:extended-string t t) t
 "@2;((#0)->string.self[fix(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:char-set :unsafe (ext:extended-string fixnum character) :char
 "(#0)->string.self[#1]= #2")

(def-inline schar :always (t t) t "ecl_elt(#0,fixint(#1))")
(def-inline schar :always (t fixnum) t "ecl_elt(#0,#1)")
(def-inline schar :always (t fixnum) :wchar "ecl_char(#0,#1)")
(def-inline schar :unsafe (base-string t) t "CODE_CHAR((#0)->base_string.self[fix(#1)])")
#-unicode
(def-inline schar :unsafe (t fixnum) :char "(#0)->base_string.self[#1]")
(def-inline schar :unsafe (base-string fixnum) :char "(#0)->base_string.self[#1]")
#+unicode
(def-inline schar :unsafe (ext:extended-string fixnum) :wchar "(#0)->string.self[#1]")

(def-inline si:schar-set :always (t t t) t "ecl_elt_set(#0,fixint(#1),#2)")
(def-inline si:schar-set :always (t fixnum t) t "ecl_elt_set(#0,#1,#2)")
(def-inline si:schar-set :always (t fixnum character) :wchar "ecl_char_set(#0,#1,#2)")
#-unicode
(def-inline si:schar-set :unsafe (t t t) t
 "@2;((#0)->base_string.self[fix(#1)]=ecl_char_code(#2),(#2))")
#-unicode
(def-inline si:schar-set :unsafe (t fixnum base-char) :char
 "(#0)->base_string.self[#1]= #2")
(def-inline si:schar-set :unsafe (base-string t t) t
 "@2;((#0)->base_string.self[fix(#1)]=ecl_char_code(#2),(#2))")
(def-inline si:schar-set :unsafe (base-string fixnum base-char) :char
 "(#0)->base_string.self[#1]= #2")
#+unicode
(def-inline si:schar-set :unsafe (ext:extended-string fixnum t) :wchar
 "@2;((#0)->string.self[#1]= ecl_char_code(#2),(#2))")
#+unicode
(def-inline si:schar-set :unsafe (ext:extended-string fixnum character) :wchar
 "(#0)->string.self[#1]= #2")

(def-inline string= :always (string string) :bool "ecl_string_eq(#0,#1)")

;; file structure.d

(def-inline si:structure-name :always (structure) symbol "SNAME(#0)")

(def-inline si:structure-ref :always (t t fixnum) t "ecl_structure_ref(#0,#1,#2)")

(def-inline si:structure-set :always (t t fixnum t) t
 "ecl_structure_set(#0,#1,#2,#3)")

;; file symbol.d

(def-inline get :always (t t t) t "ecl_get(#0,#1,#2)")
(def-inline get :always (t t) t "ecl_get(#0,#1,Cnil)")

(def-inline symbol-name :always (t) string "ecl_symbol_name(#0)")

;; AKCL addition

(proclaim-function si:copy-stream (t t) t)

;; Additions used by the compiler.
;; The following functions do not exist. They are always expanded into the
;; given C code. References to these functions are generated in the C1 phase.

(proclaim-function shift>> (*) nil :no-side-effects)
(def-inline shift>> :always (fixnum fixnum) :fixnum "((#0) >> (- (#1)))")

(proclaim-function shift<< (*) nil :no-side-effects)
(def-inline shift<< :always (fixnum fixnum) :fixnum "((#0) << (#1))")

(proclaim-function short-float-p (t) gen-bool :predicate)
#-short-float
(def-inline short-float-p :always (t) :bool "type_of(#0)==t_singlefloat")
#+short-float
(def-inline short-float-p :always (t) :bool "type_of(#0)==t_shortfloat")

(proclaim-function single-float-p (t) gen-bool :predicate)
(def-inline single-float-p :always (t) :bool "type_of(#0)==t_singlefloat")

(proclaim-function double-float-p (t) gen-bool :predicate)
(def-inline double-float-p :always (t) :bool "type_of(#0)==t_doublefloat")

(proclaim-function long-float-p (t) gen-bool :predicate)
#-long-float
(def-inline long-float-p :always (t) :bool "type_of(#0)==t_doublefloat")
#+long-float
(def-inline long-float-p :always (t) :bool "type_of(#0)==t_longfloat")

(proclaim-function si:fixnump (t) gen-bool :predicate)
(def-inline si:fixnump :always (t) :bool "FIXNUMP(#0)")
(def-inline si:fixnump :always (fixnum) :bool "1")

(proclaim-function si:put-properties (*) nil :no-sp-change)

(proclaim-function c::ldb1 (fixnum fixnum fixnum) fixnum :no-side-effects)
(def-inline c::ldb1 :always (fixnum fixnum fixnum) :fixnum
 "((((~((cl_fixnum)-1 << (#0))) << (#1)) & (cl_fixnum)(#2)) >> (#1))")
(def-inline c::ldb1 :always (fixnum fixnum fixnum) t
 "MAKE_FIXNUM((((~((cl_fixnum)-1 << (#0))) << (#1)) & (cl_fixnum)(#2)) >> (#1))")

;; Functions only available with CLOS

#+clos(progn
(proclaim-function si:allocate-raw-instance (t t fixnum) t)
(proclaim-function si:instance-ref-safe (t fixnum) t)
(proclaim-function si:instance-ref (t fixnum) t :no-side-effects)
(def-inline si:instance-ref :always (t fixnum) t "ecl_instance_ref((#0),(#1))")
(def-inline si:instance-ref :unsafe (standard-object fixnum) t
 "(#0)->instance.slots[#1]")

(proclaim-function si:instance-set (t fixnum t) t)
(def-inline si:instance-set :unsafe (t fixnum t) t
 "ecl_instance_set((#0),(#1),(#2))")
(def-inline si:instance-set :unsafe (standard-object fixnum t) t
 "(#0)->instance.slots[#1]=(#2)")

(proclaim-function si:instance-class (t) t :no-side-effects)
(def-inline si:instance-class :always (standard-object) t "CLASS_OF(#0)")
(proclaim-function si:instance-class-set (t t) t)
(proclaim-function si:instancep (t) t :predicate)
(def-inline si::instancep :always (t) :bool "@0;ECL_INSTANCEP(#0)")
(proclaim-function si:unbound (*) t :predicate)
(def-inline si:unbound :always nil t "ECL_UNBOUND")

(proclaim-function si:sl-boundp (t) t :predicate)
(def-inline si:sl-boundp :always (t) :bool "(#0)!=ECL_UNBOUND")

(proclaim-function si:sl-makunbound (t fixnum) t :predicate)

(proclaim-function standard-instance-access (standard-object fixnum) t :no-side-effects)
(def-inline standard-instance-access :always (standard-object fixnum) t "ecl_instance_ref((#0),(#1))")
(def-inline standard-instance-access :unsafe (standard-object fixnum) t
 "(#0)->instance.slots[#1]")

(proclaim-function funcallable-standard-instance-access (funcallable-standard-object fixnum) t :no-side-effects)
(def-inline funcallable-standard-instance-access :always (funcallable-standard-object fixnum) t "ecl_instance_ref((#0),(#1))")
(def-inline funcallable-standard-instance-access :unsafe (funcallable-standard-object fixnum) t
 "(#0)->instance.slots[#1]")

(proclaim-function associate-methods-to-gfun (generic-function *) generic-function)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTIONS WHICH CAN BE CALLED FROM C
;;;
;;; The following two lists contain all functions in the core library which do
;;; not belong to the C part of the library, but which should have an exported C
;;; name that users (and compiled code) can refer to. This means, for instance, that
;;; MAKE-ARRAY will be compiled to a function called cl_make_array, etc.
;;;

(in-package "SI")

(defvar c::*in-all-symbols-functions*
  '(;; arraylib.lsp
    make-array vector array-dimensions array-in-bounds-p array-row-major-index
    bit sbit bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1
    bit-andc2 bit-orc1 bit-orc2 bit-not
    vector-push vector-push-extend vector-pop adjust-array
    ;; conditions.lsp
    si::safe-eval
    signal warn break make-condition compute-restarts find-restart
    invoke-restart invoke-restart-interactively
    abort continue muffle-warning store-value use-value
    ;; config.lsp
    short-site-name long-site-name machine-instance machine-type machine-version
    software-type software-version lisp-implementation-type lisp-implementation-version
    ;; describe.lsp
    describe inspect
    ;; iolib.lsp
    read-from-string write-to-string prin1-to-string princ-to-string
    y-or-n-p yes-or-no-p dribble si::string-to-object
    ;; listlib.lsp
    union nunion intersection nintersection set-difference nset-difference
    set-exclusive-or nset-exclusive-or subsetp rassoc-if rassoc-if-not
    assoc-if assoc-if-not member-if member-if-not subst-if subst-if-not
    nsubst-if nsubst-if-not
    ;; mislib.lsp
    logical-pathname-translations load-logical-pathname-translations decode-universal-time
    encode-universal-time get-decoded-time
    ensure-directories-exist si::simple-program-error si::signal-simple-error
    ;; module.lsp
    provide require
    ;; numlib.lsp
    isqrt phase signum cis
    asin acos asinh acosh atanh ffloor fceiling ftruncate fround
    logtest byte byte-size byte-position ldb ldb-test mask-field dpb
    deposit-field
    ;; packlib.lsp
    find-all-symbols apropos apropos-list
    find-relative-package package-parent package-children
    ;; predlib.lsp
    upgraded-array-element-type upgraded-complex-part-type typep subtypep coerce
    do-deftype
    ;; seq.lsp
    make-sequence concatenate map some every notany notevery map-into
    ;; seqlib.lsp
    reduce fill replace
    remove remove-if remove-if-not delete delete-if delete-if-not
    count count-if count-if-not substitute substitute-if substitute-if-not
    nsubstitute nsubstitute-if nsubstitute-if-not find find-if find-if-not
    position position-if position-if-not remove-duplicates
    delete-duplicates mismatch search sort stable-sort merge
    complement constantly
    ;; top.lsp
    invoke-debugger
    ;; pprint.lsp
    pprint-fill copy-pprint-dispatch pprint-dispatch
    pprint-linear pprint-newline pprint-tab pprint-tabular
    set-pprint-dispatch pprint-indent .
    #-clos
    nil
    #+clos
    (;; combin.lsp
     method-combination-error
     invalid-method-error
     #-(or) standard-instance-access ; this function is a synonym for si:instance-ref
     #-(or) funcallable-standard-instance-access ; same for this one
     subclassp of-class-p
     ;; boot.lsp
     slot-boundp
     slot-makunbound
     slot-value
     slot-exists-p
     ;; generic.lsp
     ;; ensure-generic-function cannot be here because it is redefined at run time.
     ;; print.lsp
     make-load-form-saving-slots
     )
))

(proclaim
  `(si::c-export-fname #+ecl-min ,@c::*in-all-symbols-functions*
    si::ecase-error si::etypecase-error si::do-check-type
    ccase-error typecase-error-string find-documentation find-declarations
    si::search-keyword si::check-keyword si::check-arg-length
    si::dm-too-few-arguments si::dm-bad-key
    remove-documentation si::get-documentation
    si::set-documentation si::expand-set-documentation
    si::packages-iterator
    si::pprint-logical-block-helper si::pprint-pop-helper
    si::make-seq-iterator si::seq-iterator-ref si::seq-iterator-set si::seq-iterator-next
    si::structure-type-error si::define-structure
    si::coerce-to-list si::coerce-to-vector
    si::fill-array-with-seq
    #+formatter
    ,@'(
    format-princ format-prin1 format-print-named-character
    format-print-integer
    format-print-cardinal format-print-ordinal format-print-old-roman
    format-print-roman format-fixed format-exponential
    format-general format-dollars
    format-relative-tab format-absolute-tab
    format-justification
	)
    #+clos
    ,@'(;; defclass.lsp
     clos::ensure-class
     ;; combin.lsp
     clos::simple-code-walker
     ;; standard.lsp
     clos::safe-instance-ref
     clos::standard-instance-set
     ;; kernel.lsp
     clos::install-method
     clos::class-id
     clos::class-direct-superclasses
     clos::class-direct-subclasses
     clos::class-slots
     clos::class-precedence-list
     clos::class-direct-slots
     clos::default-initargs-of
     clos::generic-function-lambda-list
     clos::generic-function-argument-precedence-order
     clos::generic-function-method-combination
     clos::generic-function-method-class
     clos::generic-function-methods
     clos::method-generic-function
     clos::method-lambda-list
     clos::method-specializers
     clos::method-qualifiers
     clos::method-function
     clos::method-plist
     clos::associate-methods-to-gfun
     ;; method.lsp
     clos::pop-next-method
     )))
