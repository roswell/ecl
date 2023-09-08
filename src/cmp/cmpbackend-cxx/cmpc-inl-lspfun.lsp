;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: C -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;; Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;; Copyright (c) 2003, Juan Jose Garcia Ripoll
;;;;
;;;;    See the file 'LICENSE' for the copyright details.
;;;;

;;;;
;;;; Database for Lisp functions accessible from C.
;;;;

(in-package "COMPILER")

;;;
;;; FUNCTIONS WHICH CAN BE CALLED FROM C
;;;
;;; The following two lists contain all functions in the core library which do
;;; not belong to the C part of the library, but which should have an exported C
;;; name that users (and compiled code) can refer to. This means, for instance, that
;;; MAKE-ARRAY will be compiled to a function called cl_make_array, etc.
;;;
;;; Note that if the created C function should take only fixed
;;; arguments, a proclamation for the function type must exist so that
;;; the compiler can produce the correct function signature!
;;;

#+ecl-min
(defvar *in-all-symbols-functions*
  ;; These functions are visible from external.h and their function
  ;; objects are created in init_all_symbols from the data in
  ;; symbols_list.h
  `(;; arraylib.lsp
    cl:make-array cl:vector cl:array-dimensions cl:array-in-bounds-p cl:array-row-major-index
    cl:bit cl:sbit cl:bit-and cl:bit-ior cl:bit-xor cl:bit-eqv cl:bit-nand cl:bit-nor cl:bit-andc1
    cl:bit-andc2 cl:bit-orc1 cl:bit-orc2 cl:bit-not
    cl:vector-pop cl:adjust-array
    ;; assert.lsp
    si:do-check-type si:ecase-error si:etypecase-error
    si:wrong-type-argument si:ccase-error si:ctypecase-error
    ;; config.lsp
    cl:short-site-name cl:long-site-name cl:machine-type cl:machine-instance cl:machine-version
    cl:software-type cl:software-version cl:lisp-implementation-type cl:lisp-implementation-version
    si:lisp-implementation-vcs-id
    ;; assignment.lsp
    si:setf-definition
    ;; conditions.lsp
    si:safe-eval cl:abort cl:continue cl:muffle-warning cl:store-value cl:use-value
    si:bind-simple-restarts si:bind-simple-handlers
    si:assert-failure cl:compute-restarts cl:find-restart cl:invoke-restart
    cl:invoke-restart-interactively cl:make-condition
    ;; describe.lsp
    cl:describe cl:inspect
    ;; iolib.lsp
    cl:read-from-string cl:write-to-string cl:prin1-to-string cl:princ-to-string
    cl:y-or-n-p cl:yes-or-no-p si:string-to-object cl:dribble
    ext:make-encoding ext:load-encoding
    ;; listlib.lsp
    cl:union cl:nunion cl:intersection cl:nintersection cl:set-difference cl:nset-difference
    cl:set-exclusive-or cl:nset-exclusive-or cl:subsetp cl:rassoc-if cl:rassoc-if-not
    cl:assoc-if cl:assoc-if-not cl:member-if cl:member-if-not cl:subst-if cl:subst-if-not
    cl:nsubst-if cl:nsubst-if-not
    ;; mislib.lsp
    cl:logical-pathname-translations cl:load-logical-pathname-translations cl:decode-universal-time
    cl:encode-universal-time cl:get-decoded-time
    cl:ensure-directories-exist si:simple-program-error si:signal-simple-error
    ;; module.lsp
    cl:provide cl:require
    ;; numlib.lsp
    cl:isqrt cl:phase cl:signum cl:cis
    cl:asin cl:acos cl:asinh cl:acosh cl:atanh cl:ffloor cl:fceiling cl:ftruncate cl:fround
    cl:logtest cl:byte cl:byte-size cl:byte-position cl:ldb cl:ldb-test cl:mask-field cl:dpb
    cl:deposit-field
    ;; packlib.lsp
    cl:find-all-symbols cl:apropos cl:apropos-list
    ;; pprint.lsp
    cl:pprint-fill cl:copy-pprint-dispatch cl:pprint-dispatch
    cl:pprint-linear cl:pprint-newline cl:pprint-tab cl:pprint-tabular
    cl:set-pprint-dispatch cl:pprint-indent
    ;; predlib.lsp
    cl:upgraded-array-element-type cl:upgraded-complex-part-type cl:typep cl:subtypep cl:coerce
    si:do-deftype si:ratiop si:single-float-p si:short-float-p si:double-float-p
    si:long-float-p
    ;; process.lsp
    ext:run-program
    ext:terminate-process
    ;; seq.lsp
    cl:make-sequence cl:concatenate cl:map cl:some cl:every cl:notany cl:notevery cl:map-into cl:complement
    ;; seqlib.lsp
    cl:reduce cl:fill cl:replace
    cl:remove cl:remove-if cl:remove-if-not cl:delete cl:delete-if cl:delete-if-not
    cl:count cl:count-if cl:count-if-not cl:substitute cl:substitute-if cl:substitute-if-not
    cl:nsubstitute cl:nsubstitute-if cl:nsubstitute-if-not cl:find cl:find-if cl:find-if-not
    cl:position cl:position-if cl:position-if-not cl:remove-duplicates
    cl:delete-duplicates cl:mismatch cl:search cl:sort cl:stable-sort cl:merge cl:constantly
    si:sequence-count
    ;; setf.lsp
    si:do-defsetf si:do-define-setf-method
    ;; trace.lsp
    si:traced-old-definition

    ,@(when (member :clos *features*)
        '(;; combin.lsp
          cl:invalid-method-error
          cl:method-combination-error
          clos:compute-effective-method-function
          clos:std-compute-effective-method
          ;; defclass.lsp
          clos::ensure-class
          clos:load-defclass
          ;; kernel.lsp
          clos:std-compute-applicable-methods
          ;; method.lsp
          clos:extract-lambda-list
          clos:extract-specializer-names
          ;; predlib.lsp
          si:subclassp si:of-class-p
          ;; slotvalue.lsp
          cl:slot-makunbound
          ;; std-slot-value.lsp
          cl:slot-boundp
          cl:slot-exists-p
          cl:slot-value
          clos:slot-value-set
          clos:standard-instance-access ;; alias clos:funcallable-standard-instance-access
          clos:standard-instance-set))

    ;; cdr-5
    ext:array-index-p
    ext:negative-fixnum-p ext:non-negative-fixnum-p
    ext:non-positive-fixnum-p ext:positive-fixnum-p
    ext:negative-integer-p ext:non-negative-integer-p
    ext:non-positive-integer-p ext:positive-integer-p
    ext:negative-rational-p ext:non-negative-rational-p
    ext:non-positive-rational-p ext:positive-rational-p
    ext:negative-ratio-p ext:non-negative-ratio-p
    ext:non-positive-ratio-p ext:positive-ratio-p
    ext:negative-real-p ext:non-negative-real-p
    ext:non-positive-real-p ext:positive-real-p
    ext:negative-float-p ext:non-negative-float-p
    ext:non-positive-float-p ext:positive-float-p
    ext:negative-short-float-p ext:non-negative-short-float-p
    ext:non-positive-short-float-p ext:positive-short-float-p
    ext:negative-single-float-p ext:non-negative-single-float-p
    ext:non-positive-single-float-p ext:positive-single-float-p
    ext:negative-double-float-p ext:non-negative-double-float-p
    ext:non-positive-double-float-p ext:positive-double-float-p
    ext:negative-long-float-p ext:non-negative-long-float-p
    ext:non-positive-long-float-p ext:positive-long-float-p))

(proclaim
  ;; These functions are not visible in external.h and have no entry in
  ;; symbols_list.h
  `(si::c-export-fname
    ,@(when (member :ecl-min *features*)
        *in-all-symbols-functions*)
    ;; defmacro.lsp
    si::find-documentation si::find-declarations
    si::search-keyword si::check-keyword
    si::dm-too-many-arguments si::dm-too-few-arguments
    si::remove-documentation
    ;; defstruct.lsp
    si::structure-type-error si::define-structure
    ;; helpfile.lsp
    si::get-documentation si::set-documentation
    si::expand-set-documentation
    ;; packlib.lsp
    si::packages-iterator
    ;; pprint.lsp
    si::pprint-logical-block-helper si::pprint-pop-helper
    ;; seq.lsp
    si::make-seq-iterator si::seq-iterator-ref
    si::seq-iterator-set si::seq-iterator-next
    si::coerce-to-list si::coerce-to-vector
    ,@(when (member :formatter *features*)
        '(si::format-princ si::format-prin1 si::format-print-named-character
          si::format-print-integer
          si::format-print-cardinal si::format-print-ordinal si::format-print-old-roman
          si::format-print-roman si::format-fixed si::format-exponential
          si::format-general si::format-dollars
          si::format-relative-tab si::format-absolute-tab
          si::format-justification))
    ,@(when (member :clos *features*)
        '(;; generic.lsp
          clos::associate-methods-to-gfun
          ;; kernel.lsp
          clos::install-method
          ;; std-slot-value.lsp
          clos::find-slot-definition
          ;; clos::generic-function-lambda-list
          ;; clos::generic-function-argument-precedence-order
          ;; clos::generic-function-method-combination
          ;; clos::generic-function-method-class
          ;; clos::generic-function-methods
          ;; clos::method-generic-function
          ;; clos::method-lambda-list
          ;; clos::method-specializers
          ;; clos::method-qualifiers
          ;; clos::method-function
          ;; clos::method-plist
          ))))
