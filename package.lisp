;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:asinine 
  (:use #:cl)
  (:nicknames #:asn1)
  (:export #:pack
	   #:unpack
	   #:encode-identifier
	   #:decode-identifier
	   #:encode-length 
	   #:decode-length 
	   #:encode-boolean
	   #:decode-boolean
	   #:encode-integer
	   #:decode-integer
	   #:encode-uinteger
	   #:decode-uinteger
	   #:encode-bit-string
	   #:decode-bit-string
	   #:encode-octet-string
	   #:decode-octet-string
	   #:encode-null
	   #:decode-null
	   #:encode-general-string
	   #:decode-general-string 
	   #:encode-generalized-time
	   #:decode-generalized-time
	   #:encode-oid
	   #:decode-oid
	   #:encode-sequence-of
	   #:decode-sequence-of
	   #:encode-tagged-type
	   #:decode-tagged-type
	   #:defsequence
	   #:gen))





