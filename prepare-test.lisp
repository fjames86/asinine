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

(loop :for (pname asn) :in '(("BsxContextual" "
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
BsxPrimitive ::= CHOICE {
    pInteger INTEGER,
    pBits    BIT STRING,
    pBytes   OCTET STRING
}
BsxObject ::= CHOICE {
    null      [0] NULL,
    primitive [1] BsxPrimitive,
    cons      [2] BsxCons
}
END
")
                             ("BsxImplicit" "
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
BsxPrimitive ::= CHOICE {
    pInteger INTEGER,
    pBits    BIT STRING,
    pBytes   OCTET STRING
}
BsxObject ::= CHOICE {
    null       NULL,
    primitive  BsxPrimitive,
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
          (load (compile-file (make-pathname :name pname :type "lisp"))))
