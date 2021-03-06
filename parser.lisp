;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:asinine-parser
  (:use #:cl)
  (:nicknames #:asn1-parser)
  (:export #:parse-definition
           #:compile-definition))

(in-package #:asinine-parser)

(defun lisp-name (string)
  (with-output-to-string (s)
    (do ((i 0 (1+ i))
         (state :normal))
        ((= i (length string)))
      (let ((c (char string i)))
        ;; if the previous character was a downcased character and this one is an upcased character then
        ;; output a -
        (cond
          ((and (eq state :downcase) (upper-case-p c))
           (princ #\- s)
           (setf state :upcase))
          ((lower-case-p c)
           (setf state :downcase))
          (t (setf state :normal)))
        (princ (char-upcase c) s)))))

(cl-lex:define-string-lexer asn1-lexer
  ("SEQUENCE" (return (values 'sequence 'sequence)))
  ("SET" (return (values 'setsym 'setsym)))
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
  ("CHOICE" (return (values 'choicesym 'choicesym)))
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
  ("DEFAULT" (return (values 'defaultsym 'defaultsym)))
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
  ("\\|" (return (values 'pipe 'pipe)))
  "--(.*?)--" ;; inline comment
  "--(.*)\\\n"  ;; single line comments
  ("0x([0-9a-fA-F]+)" (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("[-]?[0-9]+" (return (values 'constant (parse-integer $@))))
  ("\\\"([0-9a-fA-F]+)\\\"" (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("[\\w-]+" (return (values 'name (alexandria:symbolicate (lisp-name $@))))) ;;substitute #\- #\_ (string-upcase $@))))))
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
  (:terminals (|::=| |,| |.| |{| |}| |(| |)| |..| |[| |]| pipe
                     integer boolean bit octet string any null
                     object identifier defaultsym
                     implicit explicit begin end definitions
                     sequence setsym of tags choicesym optional defined by
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
   (name |::=| type (lambda (a b c) (declare (ignore b)) `(,a ,c)))
   (name object identifier |::=| |{| object-identifier-list |}|
         (lambda (a b c d e f g) (declare (ignore b c d e g))
           `(,a (:object-identifier ,f))))
   (name name |::=| |{| object-identifier-list |}|
         (lambda (a b c d e f) (declare (ignore c d f))
           `(,a (:object-identifier-alias ,b ,e))))
   (name integer |::=| constant
         (lambda (a b c d) (declare (ignore b c))
           `(,a (:integer ,d)))))

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
      name
      (name |(| name |)| (lambda (a b c d) (declare (ignore b c d)) a))
    (name |(| bit-string-option-list |)|
          (lambda (a b c d) (declare (ignore b c d))
            a)))

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
            (lambda (a b c d e f) (declare (ignore a b d f)) `(:integer :range :start ,c :end ,e)))
   (integer |(| constant |..| name |)|
            (lambda (a b c d e f) (declare (ignore a b d f))
              `(:integer :range :start ,c :end ,e)))
   (integer |(| constant-list |)| (lambda (a b c d) (declare (ignore a b d)) `(:integer :member ,c)))
   (integer |{| named-number-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:integer :member ,c)))
   (integer |{| named-number-list |}| |(| constant |..| name |)|
            (lambda (a b c d e f g h i) (declare (ignore a b d e g i))
              `(:integer :member ,c :start ,f :end ,h))))

  (constant-list
   (constant (lambda (a) (list a)))
   (constant-list pipe constant (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (bit-string-range
   (constant |..| constant (lambda (a b c) (declare (ignore b)) `(:range :start ,a :end ,c)))
   (constant |..| max (lambda (a b c) (declare (ignore b c)) `(:range :start ,a :end nil)))
   (constant |..| name (lambda (a b c) (declare (ignore b))
                         `(:range :start ,a :end ,c)))
   (constant (lambda (a) `(:integer :member (,a))))
   (name (lambda (a) `(:integer :member (,a)))))

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
   (setsym |{| element-type-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:set ,c)))
   (setsym |{| |}| (lambda (a b c) (declare (ignore a b c)) `(:set nil))))

  (set-of
   (setsym of type (lambda (a b c) (declare (ignore a b)) `(:set-of ,c)))
   (setsym bit-string-option-list of type
           (lambda (a b c d) (declare (ignore a c))
             `(:set-of ,d :options ,b))))

  (choice
   (choicesym |{| alternative-type-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:choice ,c))))

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
   (name |(| constant |)| (lambda (a b c d) (declare (ignore b d)) `(:number ,a ,c)))
   (constant (lambda (a) `(:number nil ,a)))
   (name (lambda (a) `(:number ,a nil))))

  (named-bit-list
   (named-bit (lambda (a) (list a)))
   (named-bit-list |,| named-bit (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (named-bit
   (name |(| constant |)| (lambda (a b c d) (declare (ignore b d)) `(:named-bit ,a ,c))))

  (element-type-list
   (element-type (lambda (a) (list a)))
   (element-type-list |,| element-type (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (value
   name
   constant)

  (element-type
   named-type
   (named-type optional (lambda (a b) (declare (ignore b)) (append a `(:optional t))))
   (named-type defaultsym value (lambda (a b c) (declare (ignore b)) (append a `(:default ,c))))
   (named-type defined by name
               (lambda (a b c d) (declare (ignore b c))
                 (append a `(:defined-by ,d))))
   (named-type defined by name optional
               (lambda (a b c d e) (declare (ignore b c e))
                 (append a `(:defined-by ,d :optional t)))))

  (named-type
   (name type)
   (name |[| constant |]| type (lambda (a b c d e) (declare (ignore b d)) `(,a ,e :tag ,c)))
   (name |[| constant |]| implicit type
         (lambda (a b c d e f) (declare (ignore b d e))
           `(,a ,f :tag ,c :implicit t)))
   (name |[| constant |]| explicit type
         (lambda (a b c d e f) (declare (ignore b d e))
           `(,a ,f :tag ,c :explicit t)))
   (type (lambda (a) `(nil ,a))))

  (alternative-type-list
   (named-type (lambda (a) (list a)))
   (alternative-type-list |,| named-type (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (empty))

(defun test-parser (string)
  (yacc:parse-with-lexer (asn1-lexer string) *asn1-parser*))

(defun parse-definition (pathspec)
  "Parse the ASN.1 specification stored in the file named by PATHSPEC. Returns the parsed definition."
  (let ((body
            (with-open-file (f pathspec :direction :input)
              (with-output-to-string (s)
                (do ((l (read-line f nil nil) (read-line f nil nil)))
                    ((null l))
                  (princ l s)
                  (fresh-line s))))))
    (let ((asn1 (test-parser body)))
      asn1)))

(defun compile-definition (pathspec &optional outfile)
  "Parse an ASN.1 definition and generate a Lisp file with functions to encode/decode using DER.

PATHSPEC ::= the ASN.1 definition.
OUTFILE ::= name of file to put the Lisp code into, defaults to the ASN.1 definition with the extension .lisp.

Returns the parsed contents."
  (let ((*package* (find-package "ASININE"))
        (pathname (or outfile
                      (merge-pathnames (make-pathname :type "lisp")
                                       (truename pathspec)))))
    (with-open-file (f pathname :direction :output :if-exists :supersede)
      (let ((asn1 (parse-definition pathspec)))
        (asinine:gen asn1 f)
        asn1))))
