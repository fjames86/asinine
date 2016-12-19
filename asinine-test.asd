;;;; Copyright (c) Pascal J. Bourguignon <pjb@informatimago.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :asinine-test
  :name "asinine-test"
  :author "Pascal J. Bourguignon <pjb@informatimago.com>"
  :description "Tests the ASN.1 parser and code generator."
  :license "MIT"
  :serial t
  :components ((:file "test"))
  :depends-on (:asinine :asinine-parser))
