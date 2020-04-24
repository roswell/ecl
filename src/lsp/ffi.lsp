;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; FFI        Symbols used in the foreign function interface

(in-package "FFI")

(import 'si:null-pointer-p)
(export 'si:null-pointer-p)

#-ecl-min
(clines "#include <string.h>")

(defmacro def-constant (name value &key (export nil))
  "Macro to define a constant and to export it"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name ,value)
     ,(when export (list 'export `(quote ,name)))
    ',name))

;;;----------------------------------------------------------------------
;;; FOREIGN TYPES
;;;

(defparameter *ffi-types* (make-hash-table :size 128))

(defparameter *use-dffi* t)

(defmacro def-foreign-type (name definition)
  "Syntax: (def-foreign-type name definition)

Defines a new foreign type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name ffi::*ffi-types*) ',definition)))

(defmacro def-type (name definition)
  (declare (ignore definition))
  `(deftype ,name () t))

(defun %convert-to-ffi-type (type &optional context)
  (if (atom type)
    (if (member type context)
      type
      (multiple-value-bind (value present-p) (gethash type *ffi-types* type)
        (if present-p
          (%convert-to-ffi-type value (cons type context))
          value)))
    (cons (%convert-to-ffi-type (first type) context)
          (%convert-to-ffi-type (rest type) context))))

(defmacro %align-data (data align)
  `(setf ,data (* (ceiling (/ ,data ,align)) ,align)))

(defun size-of-foreign-type (name)
  "Syntax: (size-of-foreign-type ftype)

Returns the number of data bytes used by a foreign object type. This
does not include any Lisp storage overhead."
  (let* ((size 0) align
         (type (%convert-to-ffi-type name)))
    (unless type
      (error "Incomplete or unknown foreign type ~A" name))
    (cond ((symbolp type)
           (setf size (si:size-of-foreign-elt-type type)
                 align (si:alignment-of-foreign-elt-type type)))
          ((atom type)
           (error "~A is not a valid foreign type identifier" name))
          ((eq (setf name (first type)) :struct)
           (setf size (slot-position type nil)
                 align (apply #'max (mapcar #'(lambda (field)
                                                (multiple-value-bind (field-size field-align)
                                                    (size-of-foreign-type (second field))
                                                  (declare (ignore field-size))
                                                  field-align))
                                            (rest type))))
           (%align-data size align))
          ((eq name :array)
           (unless (and (setf size (third type)) (realp size))
             (error "Incomplete foreign type: ~S" type))
           (multiple-value-bind (elt-size elt-align)
             (size-of-foreign-type (second type))
             (setf size (* size elt-size)
                   align elt-align)))
          ((eq name :union)
           (dolist (field (rest type))
             (multiple-value-bind (field-size field-align)
               (size-of-foreign-type (second field))
               (when (> field-size size)
                 (setf size field-size))
               (when (or (null align) (> field-align align))
                 (setf align field-align)))))
          ((eq name '*)
           (setf size (si:size-of-foreign-elt-type :pointer-void)
                 align (si:alignment-of-foreign-elt-type :pointer-void)))
          ((eq name 'quote)
           (return-from size-of-foreign-type
             (size-of-foreign-type (second type))))
          (t
           (error "~A does not denote a foreign type" name)))
    (values size (or align 0))))

(defun allocate-foreign-object (type &optional (size 0 size-flag))
    "Syntax: (allocate-foreign-object type &optional (size 0)

Allocates an instance of a foreign object. It returns a pointer to the
object."
  (let ((type-size (size-of-foreign-type type)))
    (cond ((null size-flag)
           (si::allocate-foreign-data type type-size))
          ((and (typep size 'fixnum) (>= size 0))
           (let ((bytes (* size type-size)))
             (si::allocate-foreign-data `(:array ,type ,size) bytes)))
          (t
           (error "~A is not a valid array dimension size" size)))))

(defun free-foreign-object (ptr)
  "Syntax: (free-foreign-object ptr)

Frees memory that was allocated for a foreign object."
  (si::free-foreign-data ptr))

;;;----------------------------------------------------------------------
;;; ENUMERATION TYPES
;;;

(defmacro def-enum (name values-list &key (separator-string "#"))
  "Syntax: (def-enum name (&rest values-list) &key (separator-string \"#\")

Defines a C enumeration"
  (let ((constants '())
        (value -1)
        field
        forms)
    (setf #| name (string name) |#
          separator-string (string separator-string))
    (dolist (item values-list)
      (cond ((symbolp item)
             (setf field item)
             (incf value))
            ((and (consp item)
                  (symbolp (setf field (first item)))
                  (integerp (setf value (second item)))
                  (endp (cddr item))))
            (t
             (error "Not a valid argument to DEF-ENUM~%~a" values-list)))
      (setf field (concatenate 'string
                               (symbol-name name)
                               separator-string
                               (string field)))
      (push `(defconstant ,(intern field (symbol-package name))
               ',value)
            forms))
    `(progn
       (def-foreign-type ,name :int)
       ,@forms)))


;;;----------------------------------------------------------------------
;;; STRUCTURE TYPES
;;;
;;; The structure type is represented by the following list:
;;;
;;;     (STRUCT (SLOT-NAME1 . SLOT-TYPE1)*)
;;;
;;; FIXME! We do not care about slot alignment!
;;;

(defmacro def-struct (name &rest slots)
  "Syntax: (def-struct name (SLOT-NAME . SLOT-TYPE)*)

Defines a C structure. SLOT-TYPE is denoted by a FFI type."
  (let ((struct-type (list :struct))
        field
        type)
    (dolist (item (subst `(* ,name) :pointer-self slots))
      (if (and (consp item)
               (= (length item) 2)
               (symbolp (setf field (first item))))
        (setf type (second item))
        (error "Not a valid DEF-STRUCT slot ~A" item))
      (push (list field type) struct-type))
    `(def-foreign-type ,name ,(nreverse struct-type))))

(defun slot-position (type field)
  (setf type (%convert-to-ffi-type type))
  (let ((ndx 0)
        (is-union nil))
    (cond ((atom type)
           (error "~A is not a foreign STRUCT or UNION type" type))
          ((eq (first type) :struct))
          ((eq (first type) :union)
           (setf is-union t))
          (t
           (error "~A is not a foreign STRUCT or UNION type" type)))
    (dolist (slot (rest type))
      (let* ((slot-name (car slot))
             (slot-type (cadr slot)))
        (multiple-value-bind (slot-size slot-align)
          (size-of-foreign-type slot-type)
          (%align-data ndx slot-align)
          (when (eq slot-name field)
            (return-from slot-position (values ndx slot-type slot-size)))
          (unless is-union
            (incf ndx slot-size)))))
    (values ndx nil nil)))

(defun get-slot-value (object struct-type field)
  "Syntax: (get-slot-value object struct-type field)

Accesses a FIELD value from a OBJECT of type STRUCT-TYPE."
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (%foreign-data-ref object slot-ndx slot-type slot-size)))

(defun (setf get-slot-value) (value object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (%foreign-data-set object slot-ndx slot-type value)))

(defun get-slot-pointer (object struct-type field)
  "Syntax: (get-slot-pointer object struct-type field)

Accesses a FIELD pointer value from a OBJECT of type STRUCT-TYPE."
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (si::foreign-data-pointer object slot-ndx slot-size slot-type)))


;;;----------------------------------------------------------------------
;;; ARRAYS
;;;

(defmacro def-array-pointer (name element-type)
  `(def-foreign-type ,name (* ,element-type)))

(defun deref-array (array array-type position)
  "Syntax: (deref-array array type position)

Dereferences (retrieves) the value of the foreign ARRAY element on the
POSITION."
  (setf array-type (%convert-to-ffi-type array-type))
  (let* ((element-type (second array-type))
         (element-size (size-of-foreign-type element-type))
         (ndx (* position element-size))
         (length (or (third array-type) '*)))
    (unless (or (eq length '*)
                (> length position -1))
      (error "Out of bounds when accessing array ~A." array))
    (%foreign-data-ref (si::foreign-data-recast array (+ ndx element-size) array-type) ndx element-type element-size)))

(defun (setf deref-array) (value array array-type position)
  (setf array-type (%convert-to-ffi-type array-type))
  (let* ((element-type (second array-type))
         (element-size (size-of-foreign-type element-type))
         (ndx (* position element-size))
         (length (or (third array-type) '*)))
    (unless (or (eq length '*)
                (> length position -1))
      (error "Out of bounds when accessing array ~A." array))
    (%foreign-data-set (si::foreign-data-recast array (+ ndx element-size) array-type) ndx element-type value)))

(defun %foreign-data-set (obj ndx type value)
  (cond ((foreign-elt-type-p type)
         (si::foreign-data-set-elt obj ndx type value))
        ((atom type)
         (error "Unknown foreign primitive type: ~A" type))
        ((eq (first type) '*)
         (si::foreign-data-set-elt obj ndx :pointer-void value))
        (t
         (si::foreign-data-set obj ndx value))))

(defun %foreign-data-ref (obj ndx type &optional (size 0 size-p))
  (cond ((foreign-elt-type-p type)
         (si::foreign-data-ref-elt obj ndx type))
        ((atom type)
         (error "Unknown foreign primitive type: ~A" type))
        ((eq (first type) '*)
         (si::foreign-data-recast (si::foreign-data-ref-elt obj ndx :pointer-void)
                                  (size-of-foreign-type (second type))
                                  type))
        (t
         (si::foreign-data-ref obj ndx (if size-p size (size-of-foreign-type type)) type))))

;;;----------------------------------------------------------------------
;;; UNIONS
;;;

(defmacro def-union (name &rest slots)
  "Syntax: (def-union name (field-name field-type)*)

Defines a foreign union type."
  (let ((struct-type (list :union))
        field
        type)
    (dolist (item (subst `(* ,struct-type) :pointer-self slots))
      (unless (and (consp item)
                   (= (length item) 2)
                   (symbolp (setf field (first item))))
        (error "Not a valid DEF-UNION slot ~A" item))
      (setf type (second item))
      (push (list field type) struct-type))
    `(def-foreign-type ,name ,(nreverse struct-type))))

;;;----------------------------------------------------------------------
;;; POINTERS
;;;

(defparameter +null-cstring-pointer+ (si:allocate-foreign-data :pointer-void 0))

(defun pointer-address (ptr)
  "Syntax: (pointer-address ptr)

Returns the address as an integer of a pointer."
  (si::foreign-data-address ptr))

(defun deref-pointer (ptr ftype)
  "Syntax: (deref-pointer ptr ftype)

Returns the object to which a pointer points."
  ;; FIXME! No checking!
  (setf ftype (%convert-to-ffi-type ftype))
  (cond ((foreign-elt-type-p ftype)
         (si::foreign-data-ref-elt ptr 0 ftype))
        ((atom ftype)
         (error "Unknown foreign primitive type: ~A" ftype))
        ((eq (first ftype) '*)
         (si::foreign-data-recast (si::foreign-data-ref-elt ptr 0 :pointer-void)
                                  (size-of-foreign-type (second ftype))
                                  (second ftype)))
        (t
         (error "Cannot dereference pointer to foreign data, ~A" ptr))
  ))

(defun (setf deref-pointer) (value ptr type)
  ;; FIXME! No checking!
  (setf type (%convert-to-ffi-type type))
  (if (foreign-elt-type-p type)
      (si::foreign-data-set-elt ptr 0 type value)
      (si::foreign-data-set ptr 0 value)))

(defun make-null-pointer (ftype)
  "Syntax; (make-null-pointer ftype)

Creates a NULL pointer of a specified type."
  ;(setf ftype (%convert-to-ffi-type ftype))
  (si::allocate-foreign-data ftype 0))

(defun make-pointer (addr type)
  (c-inline (type (size-of-foreign-type type) addr) (:object :unsigned-long :unsigned-long) :object
            "ecl_make_foreign_data(#0, #1, (void*)#2)"
            :side-effects t
            :one-liner t))

#+(OR) ;; Already defined in core
(defun null-pointer-p (object)
  (si::null-pointer-p object))


;;;----------------------------------------------------------------------
;;; CHARACTERS AND STRINGS
;;;
;;; ECL always returns characters when dereferencing (:array * :char)
;;;

(defun null-char-p (char)
  "Syntax: (null-char-p char)

Tests a character for NULL value."
  (eq char #.(code-char 0)))

(defun ensure-char-character (char)
  "Syntax: (ensure-char-character object)

Ensures that a dereferenced char or integer is a lisp character."
  (cond ((characterp char) char)
        ((integerp char) (code-char char))
        (t (error "~a cannot be coerced to type CHARACTER" char))))

(defun ensure-char-integer (char)
    "Syntax: (ensure-char-integer object)

Ensures that a dereferenced char or integer is a lisp integer."
  (cond ((characterp char) (char-code char))
        ((integerp char) char)
        (t (error "~a cannot be coerced to type INTEGER" char))))

(defun ensure-char-storable (char)
  char)

(defun char-array-to-pointer (obj)
  (si::foreign-data-pointer obj 0 1 '(* :unsigned-char)))

(defmacro convert-from-cstring (object)
  "Syntax: (convert-from-cstring object)

Converts a Lisp string to a cstring. This is most often used when
processing the results of a foreign function that returns a cstring."
  object)

(defmacro convert-to-cstring (object)
  "Syntax: (convert-to-cstring object)

Converts cstring OBJECT to a Lisp string. Allocates memory."
  ;; This enforces that the string contains only as many characters as the
  ;; fill-pointer determines Since ECL always sets a 0 character after the
  ;; last element of a string, this way, the string is always zero-terminated
  `(si:copy-to-simple-base-string ,object))

(defmacro free-cstring (cstring)
  "Syntax: (free-cstring cstring)

Free memory used by CSTRING."
  cstring)

(defmacro with-cstring ((cstring string) &body body)
  "Syntax: (with-cstring (cstring string) &body body)

Binds CSTRING to a cstring created from conversion of a STRING and
evaluated the BODY. Automatically frees the CSTRING."
  `(let ((,cstring (convert-to-cstring ,string))) ,@body))

(defmacro with-cstrings (bindings &rest body)
  "Syntax: (with-cstrings ((cstring string)*) &body body)

See: WITH-CSTRING. Works similar to LET*."
  (if bindings
    `(with-cstring ,(car bindings)
      (with-cstrings ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

(defun foreign-string-length (foreign-string)
  (c-inline (foreign-string) (t) :int
            "strlen((#0)->foreign.data)"
            :side-effects nil
            :one-liner t))

(defun convert-from-foreign-string (foreign-string
                                    &key length (null-terminated-p t))
  "Syntax: (convert-from-foreign-string
         foreign-string &key length (null-terminated-p t)

Returns a Lisp string from a foreign string FOREIGN-STRING. Can
translate ASCII and binary strings."
  (cond ((and (not length) null-terminated-p)
         (setf length (foreign-string-length foreign-string)))
        ((not (integerp length))
         (error "~A is not a valid string length" length)))
  (c-inline (foreign-string length) (t fixnum) string
       "{
        cl_index length = #1;
        cl_object output = ecl_alloc_simple_base_string(length);
        memcpy(output->base_string.self, (#0)->foreign.data, length);
        @(return) = output;
        }"
       :one-liner nil
       :side-effects t))

(defun convert-to-foreign-string (string-designator)
  "Syntax: (convert-to-foreign-string string-designator)

Converts a Lisp string to a foreign string. Memory should be freed
with free-foreign-object."
  (let ((lisp-string (string string-designator)))
    (c-inline (lisp-string) (t) t
       "{
        cl_object lisp_string = #0;
        cl_index size = lisp_string->base_string.fillp;
        cl_object output = ecl_allocate_foreign_data(@(* :char), size+1);
        memcpy(output->foreign.data, lisp_string->base_string.self, size);
        output->foreign.data[size] = '\\0';
        @(return) = output;
        }"
        :one-liner nil
        :side-effects t)
    ))

(defun allocate-foreign-string (size &key (unsigned T))
  "Syntax: (allocate-foreign-string size &key (unsigned t))

Allocates space for a foreign string. Memory should be freed with
FREE-FOREIGN-OBJECT. Initial contents of the string are undefined."
  (si::allocate-foreign-data `(* ,(if unsigned :unsigned-char :char))
                             (1+ size)))

(defmacro with-foreign-string ((foreign-string lisp-string) &rest body)
  "Syntax: (with-foreign-string ((foreign-string lisp-string) &rest body)

Binds FOREIGN-STRING to a foreign string created from conversion of a
STRING and evaluated the BODY. Automatically frees the FOREIGN-STRING."
  `(let* ((,foreign-string (convert-to-foreign-string ,lisp-string)))
     (mp:without-interrupts
         (unwind-protect
              (mp:with-restored-interrupts ,@body)
           (free-foreign-object ,foreign-string)))))

(defmacro with-foreign-strings (bindings &rest body)
  "Syntax: (with-foreign-strings ((foreign-string string)*) &body body)

See: WITH-FOREIGN-STRING. Works similar to LET*."
  (if bindings
    `(with-foreign-string ,(car bindings)
      (with-foreign-strings ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

;;;----------------------------------------------------------------------
;;; MACROLOGY
;;;

(defmacro with-foreign-object ((var type) &body body)
  "Syntax: (with-foreign-object (var type) &body body)

Wraps the allocation, binding and destruction of a foreign object
around a body of code"
  `(let ((,var (allocate-foreign-object ,type)))
     (mp:without-interrupts
         (unwind-protect
              (mp:with-restored-interrupts ,@body)
           (free-foreign-object ,var)))))

(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
    `(with-foreign-object ,(car bindings)
      (with-foreign-objects ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

(defmacro with-cast-pointer (bind &body body)
  "Syntax: (with-cast-pointer (var ptr ftype) &body body)

Executes BODY with PTR cast to be a pointer to type FTYPE. VAR will be
bound to this value during the execution of body."
  (let (binding-name ptr type)
    (case (length bind)
      (2 (setf binding-name (first bind)
               ptr binding-name
               type (second bind)))
      (3 (setf binding-name (first bind)
               ptr (second bind)
               type (third bind)))
      (otherwise (error "Arguments missing in WITH-CAST-POINTER")))
    `(let ((,binding-name (si::foreign-data-pointer (si::foreign-data-recast ,ptr (size-of-foreign-type ',type) :void) 0
                                                    (size-of-foreign-type ',type)
                                                    ',type)))
       ,@body)))

;;;----------------------------------------------------------------------
;;; INTERFACE TO C FUNCTIONS AND VARIABLES
;;;

(defun lisp-to-c-name (name)
  (cond ((or (stringp name)
             (symbolp name))
         (values name (intern (string-upcase (substitute #\- #\_ (string name))))))
        ((and (consp name)
              (= (length name) 2))
         (values (first name) (second name)))))

(defun %convert-to-arg-type (type)
  (let ((type (%convert-to-ffi-type type)))
    (cond ((atom type) type)
          ((eq (first type) '*) :pointer-void)
          ((eq (first type) :array) :pointer-void)
          (t (error "Unsupported argument type: ~A" type))
    )))

(defun %convert-to-return-type (type)
  (let ((type (%convert-to-ffi-type type)))
    (cond ((atom type) type)
          ((eq (first type) '*) (second type))
          (t type))))

(defun produce-function-call (c-name nargs)
  (declare (si::c-local))
  (format nil "~a(~a)" c-name
          (subseq "#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#a,#b,#c,#d,#e,#f,#g,#h,#i,#j,#k,#l,#m,#n,#o,#p,#q,#r,#s,#t,#u,#v,#w,#x,#y,#z"
                  0 (max 0 (1- (* nargs 3))))))

;;; FIXME! We should turn this into a closure generator that produces no code.
#+DFFI
(defmacro def-lib-function (name args &key returning module (call :default))
  (multiple-value-bind (c-name lisp-name) (lisp-to-c-name name)
    (let* ((return-type (ffi::%convert-to-return-type returning))
           (return-required (not (eq return-type :void)))
           (argtypes (mapcar #'(lambda (a) (ffi::%convert-to-arg-type (second a))) args)))
      `(let ((c-fun (si::find-foreign-symbol ',c-name ,module :pointer-void 0)))
        (defun ,lisp-name ,(mapcar #'first args)
          (si::call-cfun c-fun ',return-type ',argtypes (list ,@(mapcar #'first args)) ,call))))))

(defmacro def-function (name args &key module (returning :void) (call :default))
  "Syntax: (def-function name args
                         &key module (returning :void) (call :default)

Declares a foreign function."
  (declare (ignorable call))
  #+DFFI 
  (when (and module *use-dffi*)
    (return-from def-function
      `(def-lib-function ,name ,args :returning ,returning :module ,module :call ,call)))
  (multiple-value-bind (c-name lisp-name)
      (lisp-to-c-name name)
    (let* ((arguments (mapcar #'first args))
           (arg-types (mapcar #'(lambda (type) (%convert-to-arg-type (second type))) args))
           (return-type (%convert-to-return-type returning))
           (nargs (length arguments))
           (c-string (produce-function-call c-name nargs))
           (casting-required (not (or (member return-type '(:void :cstring))
                                      (foreign-elt-type-p return-type))))
           (inline-form `(c-inline ,arguments ,arg-types
                                   ,(if casting-required :pointer-void return-type)
                                   ,c-string
                                   :one-liner t
                                   :side-effects t)))
      (when casting-required
        (setf inline-form
              `(si::foreign-data-recast ,inline-form
                                        (size-of-foreign-type ',return-type)
                                        ',return-type)))
      (when (> nargs 36)
        (error "FFI can only handle C functions with up to 36 arguments"))
      `(defun ,lisp-name (,@arguments)
         ,inline-form)
      )))

(defmacro def-foreign-var (name type module)
  "Syntax: (def-foreign-var name type module)

Defines a symbol macro which can be used to access (get and set) the
value of a variable in foreign code."
  (declare (ignorable module))
  (multiple-value-bind (c-name lisp-name)
      (lisp-to-c-name name)
    (let* ((ffi-type (%convert-to-ffi-type type))
           (can-deref (or (foreign-elt-type-p ffi-type)
                          (and (consp ffi-type)
                               (member (first ffi-type) '(* :array)))))
           (inline-form (cond #+dffi
                              ((and module *use-dffi*)
                               `(si::find-foreign-symbol ,c-name ,module ',type ,(size-of-foreign-type type)))
                              (t
                               `(c-inline () () :object
                                          ,(format nil "ecl_make_foreign_data(@~S, ~A, &~A)"
                                                   type (size-of-foreign-type type) c-name)
                                          :side-effects t :one-liner t)))))
      (if can-deref
          `(progn
             (put-sysprop ',lisp-name 'ffi-foreign-var ,inline-form)
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (define-symbol-macro ,lisp-name
                 (ffi:deref-pointer (get-sysprop ',lisp-name 'ffi-foreign-var) ',type)
                 )))
          `(defparameter ,lisp-name ,inline-form))
      )))

(defun find-foreign-library (names directories &key drive-letters types)
  "Syntax: (find-foreign-library names directories &key drive-letters type)

Finds a foreign library by searching through a number of possible
locations. Returns the path of the first found file."
  (unless (listp names)
    (setq names (list names)))
  (unless (listp directories)
    (setq directories (list directories)))
  (unless types
    (setq types #+win32 '("lib")
                #-win32 '("so" "a")))
  (unless (listp types)
    (setq types (list types)))
  (unless (listp drive-letters)
    (setq drive-letters (list drive-letters)))
  #-msvc
  (setq drive-letters '(nil))
  #+msvc
  (unless drive-letters
    (setq drive-letters '(nil)))
  (dolist (d drive-letters)
    (dolist (p directories)
      (dolist (n names)
        (dolist (e types)
          (let ((full-path (probe-file (make-pathname
                                          :device d
                                          :directory (etypecase p
                                                       (pathname (pathname-directory p))
                                                       (string (pathname-directory (parse-namestring p)))
                                                       (list p))
                                          :name n
                                          :type e))))
            (when full-path
              (return-from find-foreign-library full-path))
          )))))
  nil)

(defparameter +loaded-libraries+ nil)

(defun do-load-foreign-library (tmp &optional system-library)
  (let* ((path (cond ((pathnamep tmp) tmp)
                     ((probe-file (setf tmp (string tmp))) tmp)
                     (t (compile-file-pathname tmp :type #+msvc :lib #-msvc :dll))))
         (filename (namestring path))
         (pack (find-package "COMPILER"))
         (flag (if system-library
                   (concatenate 'string "-l" tmp)
                   filename)))
    (unless (find filename ffi::+loaded-libraries+ :test #'string-equal)
      (setf (symbol-value (intern "*LD-FLAGS*" pack))
            (concatenate 'string (symbol-value (intern "*LD-FLAGS*" pack)) " " flag))
      (setf (symbol-value (intern "*LD-BUNDLE-FLAGS*" pack))
            (concatenate 'string (symbol-value (intern "*LD-BUNDLE-FLAGS*" pack))
                         " " flag))
      (setf (symbol-value (intern "*LD-SHARED-FLAGS*" pack))
            (concatenate 'string (symbol-value (intern "*LD-SHARED-FLAGS*" pack))
                         " " flag))
      (push filename ffi::+loaded-libraries+))
    t))

(defmacro load-foreign-library (filename &key module supporting-libraries force-load
                                           system-library &environment env)
  "Syntax: (load-foreign-library filename
              &key module supporting-libraries force-load system-library)

Loads a foreign library."
  (declare (ignore module force-load supporting-libraries))
  (let ((compile-form (and (constantp filename env)
                           `((ext:with-backend
                                 :c/c++ (eval-when (:compile-toplevel)
                                          (do-load-foreign-library ,filename
                                            ,(ext:constant-form-value system-library)))
                                 :bytecodes nil))))
        (dyn-form #+dffi (when (and (not system-library) *use-dffi*)
                           `((si:load-foreign-module ,filename)))
                  #-dffi nil))
    `(progn ,@compile-form ,@dyn-form)))

;;;----------------------------------------------------------------------
;;; CALLBACKS
;;;

#-dffi
(defmacro defcallback (&rest args)
  (error "DEFCALLBACK cannot be used in interpreted forms"))

#+dffi
(defmacro defcallback (name ret-type arg-desc &body body)
  (if *use-dffi*
      (multiple-value-bind (name call-type) (if (consp name)
                                                (values-list name)
                                                (values name :default))
        (let ((arg-types (mapcar #'second arg-desc))
              (arg-names (mapcar #'first arg-desc))
              (ret-type (typecase ret-type
                          ((member nil :void)      :void)
                          ((cons (member * array)) :pointer-void)
                          (otherwise               ret-type))))
          `(si::make-dynamic-callback
            #'(ext::lambda-block ,name ,arg-names ,@body)
            ',name ',ret-type ',arg-types ,call-type)))
      (error "DEFCALLBACK cannot be used in interpreted forms when DFFI is disabled.")))

(defun callback (name)
  (let ((x (si::get-sysprop name :callback)))
    (unless x
      (error "There is no callback with name ~a" name))
    x))

;;;----------------------------------------------------------------------
;;; COMPATIBILITY WITH OLDER FFI
;;;

(defun clines (&rest c/c++-code)
  "Syntax: (clines &rest c/c++-code)

CLINES is used to inline C/C++ declarations (strings) at the beginning
of the produced C/C++ header file. Works only in the compiled code and
is required to be a toplevel form."
  (error "The special form clines cannot be used in the interpreter: ~A"
         c/c++-code))

(eval-when (:load-toplevel :execute)
  (defmacro c-inline (lisp-values arg-c-types return-type c/c++-code
                      &key (side-effects t) one-liner)
    `(error "The special form c-inline cannot be used in the interpreter: ~S"
            (list (list ,@lisp-values) ',arg-c-types ',return-type
                  ,c/c++-code
                  :side-effects ,side-effects
                  :one-liner ,one-liner)))
  (defmacro c-progn (args &rest body)
    (declare (ignore args))
    '(error "The special form c-progn cannot be used in the interpreter.")))

(defmacro definline (fun arg-types type code)
"Syntax: (definline symbol (&rest arg-types) result-type &body body) " "

DEFINLINE behaves like a DEFCBODY (see), but also instructs the LISP compiler
to expand inline any call to function SYMBOL into code corresponding
to the C language expression BODY, whenever it can determine that
the actual arguments are of the specified type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
              ;; defCbody must go first, because it clears symbol-plist of fun
              (defCbody ,fun ,arg-types ,type ,code)
              (declaim (ftype (function ,arg-types ,type) ,fun))
              (c::def-inline ,fun :always ,arg-types ,type ,code)))

(defmacro defla (name args &body body)
  "Syntax: (defla name args &body body)

Used to DEFine Lisp Alternative.  For the interpreter, DEFLA is equivalent to
DEFUN, but the compiler ignores this form."
  `(eval-when (:execute)
     (defun ,name ,args ,@body)))

(defmacro defcbody (name arg-types result-type c-expression)
  "Syntax: (defcbody name arg-types result-type c-expression)

The compiler defines a Lisp function named by NAME whose body consists
of the C code of the string C-EXPRESSION. In the C-EXPRESSION one can
reference the arguments of the function as \"#0\", \"#1\", etc.

The interpreter ignores this form.  ARG-TYPES are argument types of
the defined Lisp function and RESULT-TYPE is its return type."
  (let ((args (mapcar #'(lambda (x) (gensym)) arg-types)))
  `(defun ,name ,args
     (c-inline ,args ,arg-types ,result-type
               ,c-expression :one-liner t))))

(defmacro defentry (name arg-types c-name &key no-interrupts)
  "Syntax: (defentry name arg-types (result-type function-name)
                  &key no-interrupts)

The compiler defines a Lisp function named by NAME whose body consists
of a calling sequence to the C language function named by
FUNCTION-NAME.

The interpreter ignores this form. ARG-TYPES are argument types of
the C function and RESULT-TYPE is its return type."
  (let ((output-type :object)
        (args (mapcar #'(lambda (x) (gensym)) arg-types)))
    (if (consp c-name)
        (setf output-type (first c-name)
              c-name (second c-name)))
    (let* ((call (produce-function-call (string c-name) (length arg-types)))
           (full-text (if no-interrupts
                          (concatenate 'string
                                       "ecl_disable_interrupts();@(return)="
                                       call
                                       ";ecl_enable_interrupts();")
                          call)))
      `(defun ,name ,args
         (c-inline ,args ,arg-types ,output-type
                   ,full-text
                   :one-liner ,(not no-interrupts))))))

