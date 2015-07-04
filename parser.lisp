

(in-package #:asinine)

(cl-lex:define-string-lexer asn1-lexer 
  ("SEQUENCE" (return (values 'sequence 'sequence)))
  ("OF" (return (values 'of 'of)))
  ("INTEGER" (return (values 'integer 'integer)))
  ("BOOLEAN" (return (values 'boolean 'boolean)))
  ("BEGIN" (return (values 'begin 'begin)))
  ("END" (return (values 'end 'end)))
  ("NULL" (return (values 'null 'null)))
  ("BIT" (return (values 'bit 'bit)))
  ("STRING" (return (values 'string 'string)))
  ("OCTET" (return (values 'octet 'octet)))
  ("ANY" (return (values 'any 'any)))
  ("TAGS" (return (values 'tags 'tags)))
  ("IMPLICIT" (return (values 'implicit 'implicit)))
  ("EXPLICIT" (return (values 'explicit 'explicit)))
  ("CHOICE" (return (values 'choice 'choice)))
  ("DEFINITIONS" (return (values 'definitions 'definitions)))
  ("OBJECT" (return (values 'object 'object)))
  ("IDENTIFIER" (return (values 'identifier 'identifier)))
  ("SIZE" (return (values 'size 'size)))
  ("MAX" (return (values 'max 'max)))
  ("OPTIONAL" (return (values 'optional 'optional)))
  ("APPLICATION" (return (values 'application 'application)))
  ("UNIVERSAL" (return (values 'universal 'universal)))
  ("PRIVATE" (return (values 'private 'private)))
  ("BY" (return (values 'by 'by)))
  ("DEFINED" (return (values 'defined 'defined)))
  ("\\.\\." (return (values '|..| '|..|)))
  ("\\." (return (values '|.| '|.|)))
  ("\\:\\:\\=" (return (values '|::=| '|::=|)))
  ("\\," (return (values '|,| '|,|)))
  ("\\{" (return (values '|{| '|{|)))
  ("\\}" (return (values '|}| '|}|)))
  ("\\(" (return (values '|(| '|(|)))
  ("\\)" (return (values '|)| '|)|)))
  ("\\[" (return (values '|[| '|[|)))
  ("\\]" (return (values '|]| '|]|)))
  "--(.*)--" ;; inline comment
  "--(.*)\\\n"  ;; single line comments
  ("0x([0-9a-fA-F]+)" (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("[-]?[0-9]+" (return (values 'constant (parse-integer $@))))
  ("\\\"([0-9a-fA-F]+)\\\"" (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("[\\w-]+" (return (values 'name (alexandria:symbolicate (substitute #\- #\_ (string-upcase $@))))))
  )

(defun test-lexer (string)
  (let ((l (asn1-lexer string)))
    (do (done)
        (done)
      (multiple-value-bind (token val) (funcall l)
        (if token
            (format t "~S ~S~%" token val)
            (setf done t))))))

;; http://www.it.kau.se/cs/education/courses/dvgc02/08p4/labs/asn1_bnf.txt
(yacc:define-parser *asn1-parser*
  (:start-symbol module-definition)
  (:terminals (|::=| |,| |.| |{| |}| |(| |)| |..| |[| |]|
	       integer boolean bit octet string any null objid
	       object identifier
	       implicit explicit begin end definitions
	       sequence of tags choice optional defined by
	       application universal private
	       name constant
	       size max))

  (module-definition 
   (name maybe-oid-list definitions |::=| begin module-body end 
	 (lambda (a b c d e f g) (declare (ignore c d e g))
		 `(:module ,a ,f :oid ,b)))
   (name maybe-oid-list definitions explicit tags |::=| begin module-body end 
	 (lambda (a b c d e f g h i) (declare (ignore c d e f g i))
		 `(:module ,a ,h :oid ,b :explicit t)))
   (name maybe-oid-list definitions implicit tags |::=| begin module-body end 
	 (lambda (a b c d e f g h i) (declare (ignore c d e f g i))
		 `(:module ,a ,h :oid ,b :implicit t))))

  (maybe-oid-list 
   (|{| object-identifier-list |}| 
	(lambda (a b c) (declare (ignore a c)) b))
   empty)

  (module-body 
   assignment-list 
   empty)
  
  (assignment-list 
   (assignment (lambda (a) (list a)))
   (assignment-list assignment (lambda (a b) (append a (list b)))))

  (assignment 
   (name |::=| type (lambda (a b c) (declare (ignore b)) `(:assignment ,a ,c)))
   (name object identifier |::=| |{| object-identifier-list |}| 
	 (lambda (a b c d e f g) (declare (ignore b c d e g))
		 `(:assignment ,a (:object-identifier ,f)))))

  (type 
   external-type 
   builtin-type
   defined-type)

  (external-type 
   (name |.| name (lambda (a b c) (declare (ignore b)) `(:external-type ,a ,c))))

  (builtin-type 
   primitive-type 
   constructed-type
   tagged-type)

  (defined-type
    (name (lambda (a) `(:defined ,a)))
    (name |(| name |)| (lambda (a b c d) (declare (ignore b d)) `(:defined ,a ,c))))

  (primitive-type
   integer-expr
   boolean
   bitstr
   octetstr
   any
   null
   objid)

  (constructed-type
   sequence-expr
   sequence-of-expr
   set
   set-of
   choice)

  (tagged-type
   (tag type (lambda (a b) `(:tagged-type ,a ,b)))
   (tag implicit type (lambda (a b c) (declare (ignore b)) `(:tagged-type ,a ,c :implicit t)))
   (tag explicit type (lambda (a b c) (declare (ignore b)) `(:tagged-type ,a ,c :explicit t))))

  (integer-expr
   (integer (lambda (a) (declare (ignore a)) `(:integer)))
   (integer |(| constant |..| constant |)| 
	    (lambda (a b c d e f) (declare (ignore a b d f)) `(:integer ,c ,e)))
   (integer |(| constant |)| (lambda (a b c d) (declare (ignore a b d)) `(:integer ,c)))
   (integer |{| named-number-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:integer ,c))))

  (bit-string-range 
   (constant |..| constant (lambda (a b c) (declare (ignore b)) `(:range ,a ,c)))
   (constant |..| max (lambda (a b c) (declare (ignore b c)) `(:range ,a nil))))

  (bit-string-option
   (size |(| bit-string-range |)| (lambda (a b c d) (declare (ignore a b d)) c)))

  (bit-string-option-list 
   (bit-string-option (lambda (a) (list a)))
   (bit-string-option-list |,| bit-string-option (lambda (a b c) (declare (ignore b)) (append a (list c)))))
   
  (bitstr 
   (bit string (lambda (a b) (declare (ignore a b)) `(:bit-string)))
   (bit string |{| named-bit-list |}|
	(lambda (a b c d e) (declare (ignore a b c e)) `(:bit-string ,d)))
   (bit string |(| bit-string-option-list |)| (lambda (a b c d e) (declare (ignore a b c e)) `(:bit-string :options ,d))))

  (octetstr
   (octet string (lambda (a b) (declare (ignore a b)) :octet-string)))

  (objid 
   (object identifier (lambda (a b) (declare (ignore a b)) :object-identifier)))

  (object-identifier-list 
   (named-number (lambda (a) (list a)))
   (object-identifier-list named-number (lambda (a b) (append a (list b)))))
  
  (sequence-expr
   (sequence |{| element-type-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:sequence ,c)))
   (sequence |{| |}| (lambda (a b c) (declare (ignore a b c)) `(:sequence))))

  (sequence-of-expr
   (sequence of type (lambda (a b c) (declare (ignore a b)) `(:sequence-of ,c)))
   (sequence bit-string-option of type 
	     (lambda (a b c d) (declare (ignore a c))
		     `(:sequence-of ,d :size ,b))))

  (set 
   (set |{| element-type-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:set ,c)))
   (set |{| |}| (lambda (a b c) (declare (ignore a b c)) `(:set))))

  (set-of 
   (set of type (lambda (a b c) (declare (ignore a b)) `(:set-of ,c))))

  (choice 
   (choice |{| alternative-type-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:choice ,c))))
  
  (tag 
   (|[| class constant |]| (lambda (a b c d) (declare (ignore a d)) `(,b ,c))))

  (class 
   (universal (lambda (a) (declare (ignore a)) :universal))
   (application (lambda (a) (declare (ignore a)) :application))
   (private (lambda (a) (declare (ignore a)) :private))
   empty)

  (named-number-list 
   (named-number (lambda (a) (list a)))
   (named-number-list |,| named-number (lambda (a b c) (declare (ignore b)) (append a (list c)))))
  
  (named-number 
   (name |(| constant |)| (lambda (a b c d) (declare (ignore b d)) `(:number ,a ,c))))

  (named-bit-list 
   (named-bit (lambda (a) (list a)))
   (named-bit-list |,| named-bit (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (named-bit 
   (name |(| constant |)| (lambda (a b c d) (declare (ignore b d)) `(:named-bit ,a ,c))))

  (element-type-list 
   (element-type (lambda (a) (list a)))
   (element-type-list |,| element-type (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (element-type 
   named-type 
   (named-type optional (lambda (a b) (declare (ignore b)) (append a `(:optional t))))
   (named-type default value (lambda (a b c) (declare (ignore b)) (append a `(:default ,c))))
   (named-type defined by name
	       (lambda (a b c d) (declare (ignore b c))
		       (append a `(:defined-by ,d)))))

  (named-type
   (name type)
   (name |[| constant |]| type (lambda (a b c d e) (declare (ignore b d)) `(,a ,e :tag ,c)))
   (type (lambda (a) `(nil ,a))))

  (alternative-type-list 
   (named-type (lambda (a) (list a)))
   (alternative-type-list |,| named-type (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (empty))

(defun test-parser (string)
  (yacc:parse-with-lexer (asn1-lexer string) *asn1-parser*))



(defun gen (pathspec)
  (let ((body
	 (with-open-file (f pathspec :direction :input)
	   (with-output-to-string (s)
	     (do ((l (read-line f nil nil) (read-line f nil nil)))
		 ((null l))
	       (princ l s) 
	       (fresh-line s))))))
    (let ((forms (test-parser body)))
      forms)))


