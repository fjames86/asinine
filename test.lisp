;;;; Copyright (c) Pascal J. Bourguignon 2016 <pjb@informatimago.com>
;;;; This code is licensed under the MIT license.

;;;; Testing asinine (only CHOICE is tested for now).
;;;; Usage: (load "test.lisp")

(defpackage #:asinine.test
  (:use #:cl #:asinine-parser #:asinine)
  (:export  #:*debug* #:testing #:expect-error #:check))
(in-package #:asinine.test)

(defvar *debug*     nil)



(defvar *successes* 0)
(defvar *failures*  0)

(defmacro testing (&body body)
  `(let ((*successes* 0)
         (*failures*  0))
     (progn ,@body)
     (format t "~2%~5D tests~%~5D successes~%~5D ~:*~[failures~:;FAILURES~]~2%"
             (+ *successes* *failures*) *successes* *failures*)))

(defmacro expect-error (expression)
  `(handler-case
       ,expression
     (:no-error (value)
       (incf *failures*)
       (if *debug*
           (error "Expected an error for ~S but got result ~S"
                  ',expression value)
           (format *error-output* "~&Test failed: expected an error for ~S but got result ~S~%"
                   ',expression value)))
     (error ()
       (incf *successes*)
       :success)))

(defmacro check (expression)
  `(handler-case
       (let ((expression ,expression))
         (if expression
             (progn
               (incf *successes*)
               :success)
             (progn
               (incf *failures*)
               (if *debug*
                   (error "Test failed for ~S" ',expression)
                   (format t "~&Test failed for ~S~%" ',expression)))))
     (error (err)
       (if *debug*
           (if *debug*
               (error "Test failed for ~S, got error: ~S" ',expression err)
               (format t "~&Test failed for ~S got error: ~A~%" ',expression err))))))


;;

(loop :for (pname asn) :in '(("BsxContextual"
                              "
BsxContextual { bsx(31337) }
DEFINITIONS ::= BEGIN
BsxPDU ::= SEQUENCE {
    version INTEGER(0..127),
    object BsxObject
}
BsxCons ::= SEQUENCE {
    head BsxObject,
    tail BsxObject
}
BsxObject ::= CHOICE {
    null      [0] NULL,
    primitive [1] OCTET STRING,
    cons      [2] BsxCons
}
END
")

                             ("BsxImplicit"
                              "
BsxImplicit { bsx(31337) }
DEFINITIONS ::= BEGIN
BsxPDU ::= SEQUENCE {
    version INTEGER(0..127),
    object BsxObject
}
BsxCons ::= SEQUENCE {
    head BsxObject,
    tail BsxObject
}
BsxObject ::= CHOICE {
    null       NULL,
    primitive  OCTET STRING,
    cons       BsxCons
}
END
"))
      :for asn-path := (make-pathname :name pname :type "asn")
      :do (with-open-file (out asn-path
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
            (write-sequence asn out))
          (asinine-parser:compile-definition asn-path)
          (load (make-pathname :name pname :type "lisp")))

(in-package "BSX-CONTEXTUAL")
(import '(asinine.test:*debug* asinine.test:testing asinine.test:expect-error asinine.test:check))
;; (setf *debug* t)
(defparameter *pdu-octets*
  #(#x30 #x1D #x02 #x01  #x01 #xA2 #x18 #x30  #x16 #xA1 #x05 #x04  #x03 #x01 #x02 #x03
    #xA2 #x0D #x30 #x0B  #xA1 #x05 #x04 #x03  #x04 #x05 #x06 #xA0  #x02 #x05 #x00))
(load #.(merge-pathnames "test-choice.lisp" (or *compile-file-truename* *load-truename*)))
(load #.(merge-pathnames "test-encdec.lisp" (or *compile-file-truename* *load-truename*)))

(in-package "BSX-IMPLICIT")
(import '(asinine.test:*debug* asinine.test:testing asinine.test:expect-error asinine.test:check))
;; (setf *debug* t)
(defparameter *pdu-octets*
  #(#x30 #x13 #x02 #x01 #x01
    #x30 #x0E #x04 #x03 #x01 #x02 #x03
    #x30 #x07 #x04 #x03 #x04 #x05 #x06
    #x05 #x00))
(load #.(merge-pathnames "test-choice.lisp" (or *compile-file-truename* *load-truename*)))
(load #.(merge-pathnames "test-encdec.lisp" (or *compile-file-truename* *load-truename*)))

