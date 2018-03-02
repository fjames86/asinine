(in-package #:asinine.test)

(format t "TESTIN BSX CONTEXTUAL")
(in-package "BSX-CONTEXTUAL")
(import '(asinine.test:*debug* asinine.test:testing asinine.test:expect-error asinine.test:check))
;; (setf *debug* t)
(defparameter *pdu-octets*
  #(48 43 2 1 1 162 38 48 36 161 3 2 1 42 162 29 48 27 161 7 3 5 0 76 158 96 0 162 16 48 14 161 8 4 6 4 2 42 10 10 10 160 2 5 0))
(load #.(merge-pathnames "test-choice.lisp" (or *compile-file-truename* *load-truename*)))
(load #.(merge-pathnames "test-encdec.lisp" (or *compile-file-truename* *load-truename*)))

(format t "TESTIN BSX IMPLICIT")
(in-package "BSX-IMPLICIT")
(import '(asinine.test:*debug* asinine.test:testing asinine.test:expect-error asinine.test:check))
;; (setf *debug* t)
(defparameter *pdu-octets*
  #(48 29 2 1 1 48 24 2 1 42 48 19 3 5 0 76 158 96 0 48 10 4 6 4 2 42 10 10 10 5 0))
(load #.(merge-pathnames "test-choice.lisp" (or *compile-file-truename* *load-truename*)))
(load #.(merge-pathnames "test-encdec.lisp" (or *compile-file-truename* *load-truename*)))
