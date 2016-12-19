;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :asinine
  :name "asinine"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An ASN.1 parser and code generator."
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "der"))
  :depends-on (:alexandria :cl-lex :yacc :flexi-streams :babel :nibbles))

(asdf:defsystem :asinine-parser
  :name "asinine-parser"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An ASN.1 parser and code generator."
  :license "MIT"
  :serial t
  :components
  ((:file "parser"))
  :depends-on (:asinine :cl-lex :yacc))
