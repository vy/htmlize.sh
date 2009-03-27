;;; Copyright (c) 2007, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Meta-Object Protocol Symbols
;;

(defvar *mop-base-uri* "http://www.alu.org/mop/")

(defvar *mop-builtins*
  '(("spec2" . "dictionary.html#spec2")
    ("add-dependent" . "dictionary.html#add-dependent")
    ("add-direct-method" . "dictionary.html#add-direct-method")
    ("add-direct-subclass" . "dictionary.html#add-direct-subclass")
    ("add-method" . "dictionary.html#add-method")
    ("allocate-instance" . "dictionary.html#allocate-instance")
    ("class-" . "dictionary.html#class-")
    ("compute-applicable-methods" . "dictionary.html#compute-applicable-methods")
    ("compute-applicable-methods-using-classes" . "dictionary.html#compute-applicable-methods-using-classes")
    ("compute-class-precedence-list" . "dictionary.html#compute-class-precedence-list")
    ("compute-default-initargs" . "dictionary.html#compute-default-initargs")
    ("compute-discriminating-function" . "dictionary.html#compute-discriminating-function")
    ("compute-effective-method" . "dictionary.html#compute-effective-method")
    ("compute-effective-slot-definition" . "dictionary.html#compute-effective-slot-definition")
    ("compute-slots" . "dictionary.html#compute-slots")
    ("direct-slot-definition-class" . "dictionary.html#direct-slot-definition-class")
    ("effective-slot-definition-class" . "dictionary.html#effective-slot-definition-class")
    ("ensure-class" . "dictionary.html#ensure-class")
    ("ensure-class-using-class" . "dictionary.html#ensure-class-using-class")
    ("ensure-generic-function" . "dictionary.html#ensure-generic-function")
    ("ensure-generic-function-using-class" . "dictionary.html#ensure-generic-function-using-class")
    ("eql-specializer-object" . "dictionary.html#eql-specializer-object")
    ("extract-lambda-list" . "dictionary.html#extract-lambda-list")
    ("extract-specializer-names" . "dictionary.html#extract-specializer-names")
    ("finalize-inheritance" . "dictionary.html#finalize-inheritance")
    ("find-method-combination" . "dictionary.html#find-method-combination")
    ("funcallable-standard-instance-access" . "dictionary.html#funcallable-standard-instance-access")
    ("generic-function-" . "dictionary.html#generic-function-")
    ("class-mo-initargs" . "dictionary.html#class-mo-initargs")
    ("gf-mo-initargs" . "dictionary.html#gf-mo-initargs")
    ("initialization" . "dictionary.html#initialization")
    ("method-mo-initargs" . "dictionary.html#method-mo-initargs")
    ("initialization" . "dictionary.html#initialization")
    ("slotd-mo-initargs" . "dictionary.html#slotd-mo-initargs")
    ("intern-eql-specializer" . "dictionary.html#intern-eql-specializer")
    ("make-instance" . "dictionary.html#make-instance")
    ("make-method-lambda" . "dictionary.html#make-method-lambda")
    ("map-dependents" . "dictionary.html#map-dependents")
    ("method-" . "dictionary.html#method-")
    ("class-mo-readers" . "dictionary.html#class-mo-readers")
    ("gf-mo-readers" . "dictionary.html#gf-mo-readers")
    ("method-mo-readers" . "dictionary.html#method-mo-readers")
    ("slotd-mo-readers" . "dictionary.html#slotd-mo-readers")
    ("reader-method-class" . "dictionary.html#reader-method-class")
    ("remove-dependent" . "dictionary.html#remove-dependent")
    ("remove-direct-method" . "dictionary.html#remove-direct-method")
    ("remove-direct-subclass" . "dictionary.html#remove-direct-subclass")
    ("remove-method" . "dictionary.html#remove-method")
    ("set-funcallable-instance-function" . "dictionary.html#set-funcallable-instance-function")
    ("(setf class-name)" . "dictionary.html#(setf class-name)")
    ("(setf generic-function-name)" . "dictionary.html#(setf generic-function-name)")
    ("(setf slot-value-using-class)" . "dictionary.html#(setf slot-value-using-class)")
    ("slot-boundp-using-class" . "dictionary.html#slot-boundp-using-class")
    ("slot-definition-" . "dictionary.html#slot-definition-")
    ("slot-makunbound-using-class" . "dictionary.html#slot-makunbound-using-class")
    ("slot-value-using-class" . "dictionary.html#slot-value-using-class")
    ("specializer-direct-generic-functions" . "dictionary.html#specializer-direct-generic-functions")
    ("specializer-direct-methods" . "dictionary.html#specializer-direct-methods")
    ("standard-instance-access" . "dictionary.html#standard-instance-access")
    ("update-dependent" . "dictionary.html#update-dependent")
    ("validate-superclass" . "dictionary.html#validate-superclass")
    ("writer-method-class" . "dictionary.html#writer-method-class")))

(make-font-lock-keywords-and-hash-tables-and-uri-lookup-hooks
 lisp-mode
 *mop-base-uri*
 (*mop-builtins* *mop-builtins-table* font-lock-builtin-face))
