
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya
;;;;  Copyright (c) 1990, Giuseppe Attardi
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2023, Daniel Kochmański
;;;;
;;;;    See file 'LICENSE' for the copyright details.

(in-package #:compiler)

;;; Abstract target machine details

(defstruct machine
  (c-types '())
  host-type-hash
  sorted-types
  inline-information)

(defstruct (host-type (:constructor %make-host-type))
  (index 0)                             ; Precedence order in the type list
  (name t)
  (lisp-type t)
  (bits nil)
  (numberp nil)
  (integerp nil)
  (c-name nil)
  (to-lisp nil)
  (from-lisp nil)
  (from-lisp-unsafe nil))

(defun lisp-type-p (type)
  (subtypep type 'T))

(defun host-type-record-unsafe (host-type)
  (gethash host-type (machine-host-type-hash *machine*)))

(defun host-type-record (host-type)
  (ext:if-let ((record (gethash host-type (machine-host-type-hash *machine*))))
    record
    (cmperr "Not a valid C type name ~A" host-type)))

(defun host-type->lisp-type (name)
  (let ((output (host-type-record-unsafe name)))
    (cond (output
           (host-type-lisp-type output))
          ((lisp-type-p name) name)
          (t (error "Unknown representation type ~S" name)))))

(defun lisp-type->host-type (type)
  (cond
    ;; We expect type = NIL when we have no information. Should be fixed. FIXME!
    ((null type)
     :object)
    ((let ((r (host-type-record-unsafe type)))
       (and r (host-type-name r))))
    (t
     ;; Find the most specific type that fits
     (dolist (record (machine-sorted-types *machine*) :object)
       (when (subtypep type (host-type-lisp-type record))
         (return-from lisp-type->host-type (host-type-name record)))))))

(defun c-number-host-type-p (host-type)
  (let ((r (host-type-record-unsafe host-type)))
    (and r (host-type-numberp r))))

(defun c-integer-host-type-p (host-type)
  (let ((r (host-type-record-unsafe host-type)))
    (and r (host-type-integerp r))))

(defun c-integer-host-type-bits (host-type)
  (let ((r (host-type-record-unsafe host-type)))
    (and r (host-type-bits r))))

(defun c-number-type-p (type)
  (c-number-host-type-p (lisp-type->host-type type)))

(defun c-integer-type-p (type)
  (c-integer-host-type-p (lisp-type->host-type type)))

(defun c-integer-type-bits (type)
  (c-number-host-type-bits (lisp-type->host-type type)))

(defun host-type->c-name (type)
  (host-type-c-name (host-type-record type)))

;; These types can be used by ECL to unbox data They are sorted from
;; the most specific, to the least specific one.  All functions must
;; be declared in external.h (not internal.h) header file.
(defconstant +host-types+
  ;; host type           lisp type                c type                 convert C->Lisp                     convert Lisp->C             unbox Lisp->C (unsafe)
  '((:byte             . #1=((signed-byte 8)      "int8_t"               "ecl_make_int8_t"                   "ecl_to_int8_t"             "ecl_fixnum"))
    (:unsigned-byte    . #2=((unsigned-byte 8)    "uint8_t"              "ecl_make_uint8_t"                  "ecl_to_uint8_t"            "ecl_fixnum"))
    (:fixnum             integer                  "cl_fixnum"            "ecl_make_fixnum"                   "ecl_to_fixnum"             "ecl_fixnum")
    (:int                integer                  "int"                  "ecl_make_int"                      "ecl_to_int"                "ecl_to_int")
    (:unsigned-int       integer                  "unsigned int"         "ecl_make_uint"                     "ecl_to_uint"               "ecl_to_uint")
    (:long               integer                  "long"                 "ecl_make_long"                     "ecl_to_long"               "ecl_to_long")
    (:unsigned-long      integer                  "unsigned long"        "ecl_make_ulong"                    "ecl_to_ulong"              "ecl_to_ulong")
    (:cl-index           integer                  "cl_index"             "ecl_make_unsigned_integer"         "ecl_to_cl_index"           "ecl_fixnum")
    (:long-long          integer                  "ecl_long_long_t"      "ecl_make_long_long"                "ecl_to_long_long"          "ecl_to_long_long")
    (:unsigned-long-long integer                  "ecl_ulong_long_t"     "ecl_make_ulong_long"               "ecl_to_ulong_long"         "ecl_to_ulong_long")
    (:float              single-float             "float"                "ecl_make_single_float"             "ecl_to_float"              "ecl_single_float")
    (:double             double-float             "double"               "ecl_make_double_float"             "ecl_to_double"             "ecl_double_float")
    (:long-double        long-float               "long double"          "ecl_make_long_float"               "ecl_to_long_double"        "ecl_long_float")
    (:csfloat            si::complex-single-float "_Complex float"       "ecl_make_csfloat"                  "ecl_to_csfloat"            "ecl_csfloat")
    (:cdfloat            si::complex-double-float "_Complex double"      "ecl_make_cdfloat"                  "ecl_to_cdfloat"            "ecl_cdfloat")
    (:clfloat            si::complex-long-float   "_Complex long double" "ecl_make_clfloat"                  "ecl_to_clfloat"            "ecl_clfloat")
    (:unsigned-char      base-char                "unsigned char"        "ECL_CODE_CHAR"                     "ecl_base_char_code"        "ECL_CHAR_CODE")
    (:char               base-char                "char"                 "ECL_CODE_CHAR"                     "ecl_base_char_code"        "ECL_CHAR_CODE")
    (:wchar              character                "ecl_character"        "ECL_CODE_CHAR"                     "ecl_char_code"             "ECL_CHAR_CODE")
    (:float-sse-pack     ext::float-sse-pack      "__m128"               "ecl_make_float_sse_pack"           "ecl_unbox_float_sse_pack"  "ecl_unbox_float_sse_pack_unsafe")
    (:double-sse-pack    ext::double-sse-pack     "__m128d"              "ecl_make_double_sse_pack"          "ecl_unbox_double_sse_pack" "ecl_unbox_double_sse_pack_unsafe")
    ;; intentional       v
    (:int-sse-pack       ext::sse-pack            "__m128i"              "ecl_make_int_sse_pack"             "ecl_unbox_int_sse_pack"    "ecl_unbox_int_sse_pack_unsafe")
    (:object             t                        "cl_object"            ""                                  ""                          "")
    (:bool               t                        "bool"                 "ecl_make_bool"                     "ecl_to_bool"               "ecl_to_bool")
    ;; These types are never selected to unbox data.
    ;; They are here, because we need to know how to print them.
    (:void               t                        "void"                 nil                                 nil                         nil)
    (:pointer-void       si::foreign-data         "void*"                "ecl_make_pointer"                  "ecl_to_pointer"            "ecl_to_pointer")
    (:cstring            string                   "char*"                "ecl_cstring_to_base_string_or_nil" nil                         nil)
    (:char*              string                   "char*"                nil                                 nil                         nil)
    (:int8-t           . #1#)
    (:uint8-t          . #2#)
    (:int16-t            integer                  "ecl_int16_t"          "ecl_make_int16_t"                  "ecl_to_int16_t"            "ecl_to_int16_t")
    (:uint16-t           integer                  "ecl_uint16_t"         "ecl_make_uint16_t"                 "ecl_to_uint16_t"           "ecl_to_unt16_t")
    (:int32-t            integer                  "ecl_int32_t"          "ecl_make_int32_t"                  "ecl_to_int32_t"            "ecl_to_int32_t")
    (:uint32-t           integer                  "ecl_uint32_t"         "ecl_make_uint32_t"                 "ecl_to_uint32_t"           "ecl_to_uint32_t")
    (:int64-t            integer                  "ecl_int64_t"          "ecl_make_int64_t"                  "ecl_to_int64_t"            "ecl_to_int64_t")
    (:uint64-t           integer                  "ecl_uint64_t"         "ecl_make_uint64_t"                 "ecl_to_uint64_t"           "ecl_to_uint64_t")
    (:short              integer                  "short"                "ecl_make_short"                    "ecl_to_short"              "ecl_fixnum")
    (:unsigned-short     integer                  "unsigned short"       "ecl_make_ushort"                   "ecl_to_ushort"             "ecl_fixnum")))


;; FIXME number of bits is used for bit fiddling optimizations. That
;; information should be defined separately. -- jd 2019-11-27
(defconstant +this-machine-c-types+
  ;; type                                 integer bits (negative means "signed")
  '((:byte                               . -8)
    (:unsigned-byte                      .  8)
    (:unsigned-short                     . #.(- (logcount ffi:c-ushort-max)))
    (:short                              . #.(- (logcount ffi:c-ushort-max)))
    (:unsigned-int                       . #.(logcount ffi:c-uint-max))
    (:int                                . #.(- (logcount ffi:c-uint-max)))
    (:unsigned-long                      . #.(logcount ffi:c-ulong-max))
    (:long                               . #.(- (logcount ffi:c-ulong-max)))
    #+long-long     (:unsigned-long-long . #.(logcount ffi:c-ulong-long-max))
    #+long-long     (:long-long          . #.(- (logcount ffi:c-ulong-long-max)))
    (:cl-index                           . #.(logcount most-positive-fixnum))
    (:fixnum                             . #.(- -1 (logcount most-positive-fixnum)))
    (:uint8-t                            .   8)
    (:int8-t                             .  -8)
    #+:uint16-t     (:uint16-t           .  16)
    #+:uint16-t     (:int16-t            . -16)
    #+:uint32-t     (:uint32-t           .  32)
    #+:uint32-t     (:int32-t            . -32)
    #+:uint64-t     (:uint64-t           .  64)
    #+:uint64-t     (:int64-t            . -64)
    #+:sse2         (:float-sse-pack     . nil)
    #+:sse2         (:double-sse-pack    . nil)
    #+:sse2         (:int-sse-pack       . nil)
    #+complex-float (:csfloat            . nil)
    #+complex-float (:cdfloat            . nil)
    #+complex-float (:clfloat            . nil)))

(defconstant +all-machines-c-types+
  '((:object)
    (:float)
    (:double)
    (:long-double)
    (:char)
    (:unsigned-char)
    (:wchar)
    (:char*)
    (:cstring)
    (:bool)
    (:void)
    (:pointer-void)))

(defun make-host-type (all-c-types name lisp-type c-name &optional to-lisp from-lisp from-lisp-unsafe)
  (let* ((record (assoc name all-c-types))
         (bits (cdr record)))
    (when record
      ;; For integer bits we get extra information from ALL-C-TYPES
      (when bits
        (if (plusp bits)
            (setf lisp-type `(unsigned-byte ,bits))
            (setf bits (- bits)
                  lisp-type `(signed-byte ,bits))))
      (%make-host-type
       :name name
       :lisp-type lisp-type
       :bits bits
       :numberp (subtypep lisp-type 'number)
       :integerp (subtypep lisp-type 'integer)
       :c-name c-name
       :to-lisp to-lisp
       :from-lisp from-lisp
       :from-lisp-unsafe from-lisp-unsafe))))

(defun default-machine ()
  (let* ((all-c-types (append +this-machine-c-types+ +all-machines-c-types+))
         (table (make-hash-table :size 128 :test 'eq))
         (sorted-host-types
          ;; Create the host-type objects
          (loop for i from 0
             for record in +host-types+
             for host-type = (apply #'make-host-type all-c-types record)
             when host-type
             do (setf (host-type-index host-type) i)
             and collect (setf (gethash (host-type-name host-type) table) host-type))))
    ;; hack: sse-pack -> int, but int -> int-sse-pack
    (let ((r (gethash :int-sse-pack table)))
      (when r
        (setf (host-type-index r) 'ext:int-sse-pack)))
    ;; On a second pass, we replace types with more general ones
    (loop with fixnum-host-type = (gethash ':fixnum table)
       with fixnum-lisp-type = (host-type-lisp-type fixnum-host-type)
       for (name . rest) in +host-types+
       for r = (gethash name table)
       when (and r (subtypep (host-type-lisp-type r) fixnum-lisp-type))
       do (setf (host-type-from-lisp-unsafe r) "ecl_fixnum"))
    ;; Create machine object
    (make-machine :c-types all-c-types
                  :host-type-hash table
                  :sorted-types sorted-host-types)))

(defun machine-c-type-p (name)
  (gethash name (machine-host-type-hash *machine*)))

(defun machine-fixnump (number)
  (typep number (host-type-lisp-type (gethash :fixnum number))))

(defvar *default-machine* (setf *machine* (default-machine)))
