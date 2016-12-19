;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; CHOICE implementation: Copyright (c) Pascal J. Bourguignon 2016 <pjb@informatimago.com>
;;;; This code is licensed under the MIT license.

(in-package #:asinine)

(defun pack (encoder value)
  (flexi-streams:with-output-to-sequence (s)
    (funcall encoder s value)))

(defun unpack (decoder buffer)
  (flexi-streams:with-input-from-sequence (s buffer)
    (funcall decoder s)))


(defun encode-identifier (stream tag &key (class :universal) (primitive t))
  (declare (type (integer 0 30) tag))
  (write-byte (logior tag
                      (ash (ecase class
                             (:universal 0)
                             (:application 1)
                             (:context 2)
                             (:private 3))
                           6)
                      (if primitive 0 32))
              stream))

(defun decode-identifier (stream)
  "Returns (values tag class primitive-p)."
  (let ((octet (read-byte stream)))
    (values (logand octet 31)
            (ecase (ash octet -6)
              (0 :universal)
              (1 :application)
              (2 :context)
              (3 :private))
            (if (zerop (logand octet 32))
                t
                nil))))

(defun encode-length (stream length)
  "Returns the number of bytes written."
  (cond
    ((<= length 127)
     (write-byte length stream)
     1)
    (t
     ;; if the length is >127, then we split
     ;; the length into the smallest number of octets, big-endian.
     ;; we write the number of octets|#80 then the octets
     (let (octets)
       ;; split into big-endian octets
       (do ((len length))
           ((zerop len))
         (push (the (unsigned-byte 8)
                    (logand len #xff))
               octets)
         (setf len (ash len -8)))
       (write-byte (logior (length octets) #x80) stream)
       (write-sequence octets stream)))))

;; we need to put a maximum length on the decoded length because otherwise
;; we might end up attempting to allocate a silly amount, exhausting the heap
;; and potentially killing us.
(defconstant +silly-length+ (* 50 1024 1024)
  "Maximum length allowed to be decoded.")

(defun decode-length (stream)
  (let ((first (read-byte stream)))
    (cond
      ((zerop (logand first 128))
       first)
      (t
       ;; the first octet is the number of octets|#x80
       (do ((n (logand first (lognot #x80)) (1- n))
            (len 0))
           ((zerop n)
            (prog1 len
              (when (> len +silly-length+)
                (error "A silly length of ~D was decoded" len))))
         (let ((byte (read-byte stream)))
           (setf len (logior (ash len 8) byte))))))))

;; -------------- booleans ---------------

(defun encode-boolean (stream value)
  (encode-identifier stream 0)
  (encode-length stream 1)
  (write-byte (if value #xff 0) stream))

(defun decode-boolean (stream)
  (decode-identifier stream)
  (decode-length stream)
  (let ((byte (read-byte stream)))
    (if (zerop byte)
        nil
        t)))

;; ---------------------------------------

(defun decode-integer (stream)
  (decode-identifier stream)
  (let ((n (decode-length stream))
        (v (nibbles:make-octet-vector 4)))
    (dotimes (i n)
      (setf (aref v (+ (- 4 n) i)) (read-byte stream)))
    ;; check the sign bit, if it's set then this is a -ve number
    ;; and we need to fill out the rest with #xff
    (when (logtest (aref v (- 4 n)) #x80)
      ;; -ve number
      (dotimes (i (- 4 n))
        (setf (aref v i) #xff)))
    (nibbles:sb32ref/be v 0)))

(defun encode-integer (stream int)
  (encode-identifier stream 2)
  (let ((v (nibbles:make-octet-vector 4)))
    (setf (nibbles:sb32ref/be v 0) int)
    ;; we need to use the minimal number of octets
    (let ((len
            (cond
              ((and (>= int (- (expt 2 7))) (< int (expt 2 7)))
               1)
              ((and (>= int (- (expt 2 15))) (< int (expt 2 15)))
               2)
              ((and (>= int (- (expt 2 23))) (< int (expt 2 23)))
               3)
              (t 4))))
      (encode-length stream len)
      (write-sequence v stream :start (- 4 len)))))

(defun decode-uinteger (stream)
  (decode-identifier stream)
  (let ((n (decode-length stream))
        (v (nibbles:make-octet-vector 4)))
    (dotimes (i n)
      (setf (aref v (+ (- 4 n) i)) (read-byte stream)))
    (nibbles:ub32ref/be v 0)))

(defun encode-uinteger (stream int)
  (encode-identifier stream 2)
  (let ((v (nibbles:make-octet-vector 4)))
    (setf (nibbles:ub32ref/be v 0) int)
    ;; we need to use the minimal number of octets
    (let ((len
            (cond
              ((< int (expt 2 8))
               1)
              ((< int (expt 2 16))
               2)
              ((< int (expt 2 24))
               3)
              (t 4))))
      (encode-length stream len)
      (write-sequence v stream :start (- 4 len)))))

;; ----------------------------------

(defun reverse-octet (octet)
  (let ((i 0))
    (dotimes (j 8)
      (setf i (logior (ash i 1) (mod octet 2))
            octet (ash octet -1)))
    i))

;; bitstrings are in reversed ordering
;; so bit 7 of octet 0 is bit 0,
;; bit 0 of octet 0 is bit 7,
;; bit 0 of octet 1 is bit 8,
;; bit 7 of octet 1 is bit 15
;; etc
;; we need to conver the integer to little-endian order,
;; then reverse the bits of each octet
(defun encode-bit-string (stream integer)
  (let ((octets (mapcar #'reverse-octet
                        (let ((octets (nibbles:make-octet-vector 4)))
                          (setf (nibbles:ub32ref/le octets 0) integer)
                          (coerce octets 'list)))))
    (encode-identifier stream 3)
    (encode-length stream (1+ (length octets)))
    (write-byte 0 stream) ;; the number of unused bits -- always zero for us since we write octets
    (dolist (octet octets)
      (write-byte octet stream))))

(defun decode-bit-string (stream)
  (decode-identifier stream)
  (let ((n (1- (decode-length stream))))
    (read-byte stream)
    (let ((octets (loop :for i :below n
                        :collect (read-byte stream))))
      (nibbles:ub32ref/le (let ((v (nibbles:make-octet-vector 4)))
                            (dotimes (i n)
                              (setf (aref v i) (reverse-octet (nth i octets))))
                            v)
                          0))))

;; ------------------------

(defun encode-octet-string (stream octets)
  (encode-identifier stream 4)
  (encode-length stream (length octets))
  (etypecase octets
    (vector
     (dotimes (i (length octets))
       (write-byte (aref octets i) stream)))
    (list
     (dolist (octet octets)
       (write-byte octet stream)))))

(defun decode-octet-string (stream)
  (decode-identifier stream)
  (let* ((n (decode-length stream))
         (octets (nibbles:make-octet-vector n)))
    (dotimes (i n)
      (setf (aref octets i) (read-byte stream)))
    octets))

;; --------------------------

(defun encode-null (stream ignore)
  (declare (ignore ignore))
  (encode-identifier stream 5)
  (encode-length stream 0))

(defun decode-null (stream)
  (decode-identifier stream)
  (decode-length stream))

;; ------------------------

(defun encode-general-string (stream string)
  (encode-identifier stream 27)
  (let ((octets (babel:string-to-octets string)))
    (encode-length stream (length octets))
    (write-sequence octets stream)))

(defun decode-general-string (stream)
  (decode-identifier stream)
  (let ((length (decode-length stream)))
    (let ((octets (nibbles:make-octet-vector length)))
      (read-sequence octets stream)
      (babel:octets-to-string octets))))

;; -------------------------

(defun time-string (time)
  (multiple-value-bind (sec min hour day month year) (decode-universal-time time 0)
    (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
            year month day hour min sec)))

;; FIXME: the string we are given may have a Z<timezone-offset> appended.
;; if it does we should pass that to the encode-universal-time function.
(defun string-time (string)
  (let ((year (subseq string 0 4))
        (month (subseq string 4 6))
        (day (subseq string 6 8))
        (hour (subseq string 8 10))
        (min (subseq string 10 12))
        (sec (subseq string 12 14)))
    (encode-universal-time (parse-integer sec)
                           (parse-integer min)
                           (parse-integer hour)
                           (parse-integer day)
                           (parse-integer month)
                           (parse-integer year)
                           0)))

(defun decode-generalized-time (stream)
  (decode-identifier stream)
  (let ((length (decode-length stream)))
    (let ((octets (nibbles:make-octet-vector length)))
      (read-sequence octets stream)
      (string-time (babel:octets-to-string octets)))))

(defun encode-generalized-time (stream time)
  (encode-identifier stream 24)
  (let ((octets (babel:string-to-octets (time-string time))))
    (encode-length stream (length octets))
    (write-sequence octets stream)))

;; ---------------------------

;; object identifiers, these are sometimes needed as headers for other messages
;; when encapsulated in other protocols (e.g. gss???)

(defun encode-oid (stream oid)
  (encode-identifier stream 6)
  (let ((bytes
          (flexi-streams:with-output-to-sequence (s)
            (let ((b1 (car oid))
                  (b2 (cadr oid)))
              (write-byte (logior (* b1 40) b2) s)
              (dolist (b (cddr oid))
                (cond
                  ((<= b 127)
                   (write-byte b s))
                  (t
                   ;; if > 127, then we write multiple 7-bit bytes, 1st byte or'd with #x80
                   (do ((bytes nil)
                        (num b (ash num -7)))
                       ((zerop num)
                        (do ((%bytes bytes (cdr %bytes)))
                            ((null %bytes))
                          (if (null (cdr %bytes))
                              ;; last one
                              (write-byte (car %bytes) s)
                              ;; all others, or with #x80
                              (write-byte (logior (car %bytes) #x80) s))))
                     (push (logand num #x7f) bytes)))))))))
    (encode-length stream (length bytes))
    (write-sequence bytes stream)))

(defun decode-oid (stream)
  "Decode an Object Identifier (OID) which is a list of integers."
  (decode-identifier stream)
  (let ((length (decode-length stream)))
    (let ((bytes (nibbles:make-octet-vector length)))
      (flexi-streams:with-input-from-sequence (s bytes :start 1) (read-sequence bytes stream)
        (do ((oid (list (truncate (aref bytes 0) 40) (mod (aref bytes 0) 40))))
            ((= (file-position s) length) oid)
          ;; collect until <= 127
          (do ((num 0)
               (done nil))
              (done (setf oid (append oid (list num))))
            (let ((b (read-byte s)))
              (cond
                ((<= b 127)
                 (setf num (logior (ash num 7) b)
                       done t))
                (t
                 (setf num (logior (ash num 7)
                                   (logand b #x7f))))))))))))

;; ----------------------------

(defun encode-sequence-of (stream encoder values &key (tag 16) (class :universal) (primitive nil))
  (let ((bytes (flexi-streams:with-output-to-sequence (s)
                 (dolist (value values)
                   (funcall encoder s value)))))
    (encode-identifier stream tag :class class :primitive primitive)
    (encode-length stream (length bytes))
    (write-sequence bytes stream)))

(defun decode-sequence-of (stream decoder)
  (decode-identifier stream)
  (let ((length (decode-length stream)))
    (let ((bytes (nibbles:make-octet-vector length)))
      (read-sequence bytes stream)
      (flexi-streams:with-input-from-sequence (s bytes)
        (do ((values nil))
            ((not (listen s))
             (nreverse values))
          (push (funcall decoder s) values))))))

;; -----------------------------------------

(defun encode-tagged-type (stream encoder value &key class tag)
  (encode-identifier stream tag :class class :primitive nil)
  (let ((bytes (flexi-streams:with-output-to-sequence (s)
                 (funcall encoder s value))))
    (encode-length stream (length bytes))
    (write-sequence bytes stream)))

(defun decode-tagged-type (stream decoder)
  (decode-identifier stream)
  (decode-length stream)
  (funcall decoder stream))

;; -----------------------------------------

(defun encoder-name (name)
  (alexandria:symbolicate 'encode- name))

(defun decoder-name (name)
  (alexandria:symbolicate 'decode- name))

(defun accessor-name (sname name)
  (alexandria:symbolicate sname '- name))

(defmacro defsequence (name (&optional class tag) &rest slots)
  `(progn
     (defstruct ,name ,@(mapcar #'car slots))
     (defun ,(encoder-name name) (stream value)
       (declare (type ,name value)
                (type stream stream))
       (flet ((enc (stream)
                (let ((bytes (flexi-streams:with-output-to-sequence (s)
                               ,@(mapcar (lambda (slot)
                                           (destructuring-bind (s-name s-type &key tag optional) slot
                                             `(flet ((enc (stream v)
                                                       ,@(cond
                                                           ((symbolp s-type)
                                                            `((,(encoder-name s-type) stream v)))
                                                           ((eq (car s-type) :integer)
                                                            `((encode-integer stream v)))
                                                           ((eq (car s-type) :sequence-of)
                                                            `((encode-sequence-of stream
                                                                                  #',(encoder-name (cadr s-type))
                                                                                  v))))))
                                                ,(cond
                                                   (tag
                                                    `(let ((v (,(accessor-name name s-name) value)))
                                                       (when ,(if optional 'v 't)
                                                         (encode-tagged-type s #'enc v
                                                                             :class :context
                                                                             :tag ,tag))))
                                                   (t
                                                    `(enc s (,(accessor-name name s-name) value)))))))
                                         slots))))
                  (encode-identifier stream 16 :primitive nil)
                  (encode-length stream (length bytes))
                  (write-sequence bytes stream))))
         ,(if (and class tag)
              `(encode-tagged-type stream #'enc value
                                   :class ,class :tag ,tag)
              `(enc stream))))
     (defun ,(decoder-name name) (stream)
       (declare (type stream stream))
       (flet ((dec (stream)
                (decode-identifier stream)
                (let ((value (,(alexandria:symbolicate 'make- name))))
                  ,(if (some (lambda (slot) (find :tag slot)) slots)
                       `(let ((v (nibbles:make-octet-vector (decode-length stream))))
                          (read-sequence v stream)
                          (flexi-streams:with-input-from-sequence (stream v)
                            (do ()
                                ((not (listen stream)))
                              (let ((tag (decode-identifier stream)))
                                (decode-length stream)
                                (ecase tag
                                  ,@(mapcar (lambda (slot)
                                              (destructuring-bind (s-name s-type &key tag optional) slot
                                                (declare (ignore optional))
                                                `(,tag
                                                  (flet ((dec (stream)
                                                           ,@(cond
                                                               ((symbolp s-type)
                                                                `((,(decoder-name s-type) stream)))
                                                               ((eq (car s-type) :integer)
                                                                `((decode-integer stream)))
                                                               ((eq (car s-type) :sequence-of)
                                                                `((decode-sequence-of stream
                                                                                      #',(decoder-name (cadr s-type))))))))
                                                    (setf (,(accessor-name name s-name) value)
                                                          (dec stream))))))
                                     slots))))))
                       `(progn
                          (decode-length stream)
                          ,@(mapcar (lambda (slot)
                                      (destructuring-bind (s-name s-type) slot
                                        `(flet ((dec (stream)
                                                  ,@(cond
                                                      ((symbolp s-type)
                                                       `((,(decoder-name s-type) stream)))
                                                      ((eq (car s-type) :integer)
                                                       `((decode-integer stream)))
                                                      ((eq (car s-type) :sequence-of)
                                                       `((decode-sequence-of stream
                                                                             #',(decoder-name (cadr s-type))))))))
                                           (setf (,(accessor-name name s-name) value)
                                                 (dec stream)))))
                                    slots)))
                  value)))
         ,(if (and class tag)
              `(decode-tagged-type stream #'dec)
              `(dec stream))))
     ',name))


;; -----------------------------------

;; In CHOICE each alternative must have a different type, or a different tag.
;; The encoding is merely the value of the alternative.
;; We define a structure to represent a CHOICE with an identified named alternative.
;; The alternative name is converted to a KEYWORD.
;; We generate an accessor for each alternative: the writer sets the alternative, the reader checks it.
;;
;;
;; (defchoice object ()
;;   (null     null)
;;   (integer  integer)
;;   (data     octet-stream))
;; -->
;; (defstruct object
;;   (%alternative nil :type (member nil :null :integer :data))
;;   (%value))
;;
;; (defun make-null-object    (value))
;; (defun make-integer-object (value))
;; (defun make-data-object    (value))
;;
;; (defun get-object-alternative  (object)) ; we use the get- prefix to avoid collision with CHOICE alternatives.
;; (defun get-object-value        (object))
;;
;; (defun object-null    (object))
;; (defun object-integer (object))
;; (defun object-data    (object))
;;
;; (defun (setf object-null)    (new-value object))
;; (defun (setf object-integer) (new-value object))
;; (defun (setf object-data)    (new-value object))
;;
;;
;; In general, we don't know for a given type a (valid) prototype
;; value.  Therefore we cannot initialize a structure with default
;; values: we have to let the slots initialized to NIL.  And we
;; don't have a map of ASN.1 types to lisp types, so we cannot type
;; the slots either.
;;
;; So:
;; -->
;; (defstruct object
;;   %alternative
;;   %value)
;;
;; Regarding the encoding/decoding of CHOICE:
;;
;; * CHOICE with explicit tags generates contextual tag prefixes before
;;   the actual alternative data:
;;
;;     BSX { bsx(31337) } DEFINITIONS ::= BEGIN
;;     BsxPDU ::= SEQUENCE {
;;         version INTEGER(0..127),
;;         object BsxObject
;;     }
;;     BsxCons ::= SEQUENCE {
;;         head BsxObject,
;;         tail BsxObject
;;     }
;;     BsxObject ::= CHOICE {
;;         null      [0] NULL,
;;         primitive [1] OCTET STRING,
;;         cons      [2] BsxCons
;;     }
;;     END
;;
;;     (0 :-> bsx-object null null)
;;     (1 :-> bsx-object primitive :octet-string)
;;     (2 :-> bsx-object cons bsx-cons)
;;     30 1D 02 01 01 A2 18 30 16 A1 05 04 03 01 02 03 A2 0D 30 0B A1 05 04 03 04 05 06 A0 02 05 00
;;
;;
;; * CHOICE with implicit tags requires that each alternative has a
;;   distinct tag.  For this we need to keep all the type definitions
;;   specified (*TYPE* hashtable), so we may retrieve them and use their
;;   specific tags.  In this case, the alternatives can be generated in
;;   place of the CHOICE without overhead.
;;
;;     BSX { bsx(31337) } DEFINITIONS ::= BEGIN
;;     BsxPDU ::= SEQUENCE {
;;         version INTEGER(0..127),
;;         object BsxObject
;;     }
;;     BsxCons ::= SEQUENCE {
;;         head BsxObject,
;;         tail BsxObject
;;     }
;;     BsxObject ::= CHOICE {
;;         null       NULL,
;;         primitive  OCTET STRING,
;;         cons       BsxCons
;;     }
;;     END
;;
;;     (5 :-> bsx-object null null)
;;     (4 :-> bsx-object primitive :octet-string)
;;     (16 :-> bsx-object cons bsx-cons)
;;     30 13 02 01 01 30 0E 04 03 01 02 03 30 07 04 03 04 05 06 05 00


(defun valid-alternatives-p (alternatives)
  "Validate the alternatives of a CHOICE
Either we have tags, and they must be unique, or we don't have tags, and the type names must be unique."
  (cond
    ((every (lambda (alternative) (let ((tag (getf (cddr alternative) :tag))) (and (integerp tag) (<= 0 tag))))
            alternatives)
     (= (length (delete-duplicates (mapcar (lambda (alternative) (getf (cddr alternative) :tag)) alternatives)
                                   :test (function =)))
        (length alternatives)))
    ((notany (lambda (alternative) (getf (cddr alternative) :tag)) alternatives)
     (= (length (delete-duplicates (mapcar (function cadr) alternatives)))
        (length alternatives)))))

(defun %constructor-name (name)
  (alexandria:symbolicate '%make- name))

(defun constructor-name (choice-name alternative-name)
  (alexandria:symbolicate 'make- alternative-name '- choice-name))

(defun getter-name (choice-name slot-name)
  (alexandria:symbolicate 'get- choice-name '- slot-name))

(defmacro defchoice (name (&optional class tag) &rest alternatives)
  (unless (valid-alternatives-p alternatives)
    (error "Duplicate choices in the alternatives for ~A:~%~{   ~S~%~}" name alternatives))
  (let ((%constructor (%constructor-name name)))
    `(progn
       (defstruct (,name (:constructor ,%constructor (%alternative %value))) %alternative %value)
       ,@(mapcan (lambda (alternative)
                   (let ((alternative-keyword (alexandria:make-keyword (cadr alternative)))
                         (reader              (accessor-name name (car alternative))))
                     `(
                       ;; public constructor for alternative: (make-object-integer 42)
                       (defun ,(constructor-name name (car alternative)) (value)
                         (,%constructor ',alternative-keyword value))
                       ;; public reader for alternative: (object-integer choice) -> 42 or error.
                       (defun ,reader (choice)
                         (assert (eq ',alternative-keyword (,(accessor-name name '%alternative) choice))
                                 (choice)
                                 "Current alternative of CHOICE is ~S, not ~S, cannot apply ~S"
                                 (,(accessor-name name '%alternative) choice)
                                 ',alternative-keyword ',reader)
                         (,(accessor-name name '%value) choice))
                       ;; public writer for alternative: (setf (object-integer choice) 42) ; updates the %alternative too.
                       (defun (setf ,reader) (new-value choice)
                         (setf (,(accessor-name name '%alternative) choice) ',alternative-keyword
                               (,(accessor-name name '%value)       choice) new-value))
                       )))
                 alternatives)
       ;; public readers for low level slots:
       (defun ,(getter-name name 'alternative) (choice) (,(accessor-name name '%alternative) choice))
       (defun ,(getter-name name 'value)       (choice) (,(accessor-name name '%value)       choice))
       ;; DER encoding:
       (defun ,(encoder-name name) (stream choice)
         (check-type stream stream)
         (check-type choice ,name)
         (locally (declare (type ,name choice)
                           (type stream stream))
           (flet ((enc (stream)
                    (case (,(accessor-name name '%alternative) choice)
                      ,@(mapcar (lambda (alternative)
                                  (destructuring-bind (s-name s-type &key tag) alternative
                                    `((,(alexandria:make-keyword s-type))
                                      (flet ((enc (stream v)
                                               ,@(cond
                                                   ((symbolp s-type)
                                                    `((,(encoder-name s-type) stream v)))
                                                   ((eq (car s-type) :integer)
                                                    `((encode-integer stream v)))
                                                   ((eq (car s-type) :sequence-of)
                                                    `((encode-sequence-of stream
                                                                          #',(encoder-name (cadr s-type))
                                                                          v))))))
                                        ,(if tag
                                             `(encode-tagged-type stream #'enc
                                                                  (,(accessor-name name '%value) choice)
                                                                  :class :context
                                                                  :tag ,tag)
                                             `(enc stream (,(accessor-name name '%value) choice)))))))
                         alternatives))))
             ,(if (and class tag)
                  `(encode-tagged-type stream #'enc (,(accessor-name name '%value) choice)
                                       :class ,class :tag ,tag)
                  `(enc stream)))))
       ;; DER decoding:
       (defun ,(decoder-name name) (stream)
         (check-type stream stream)
         (locally (declare (type stream stream))
           (flet ((dec (stream)
                    ,(let* ((tagged-alternatives
                              (some (lambda (alternative) (getf (cddr alternative) :tag)) alternatives))
                            (gen-alternative-clause
                              (flet ((gen-alternative-decoder (name s-name s-type tag)
                                       (print (list tag :-> name s-name s-type))
                                       `(,tag
                                         (flet ((dec (stream)
                                                  ,@(cond
                                                      ((symbolp s-type)
                                                       `((,(decoder-name s-type) stream)))
                                                      ((eq (car s-type) :integer)
                                                       `((decode-integer stream)))
                                                      ((eq (car s-type) :sequence-of)
                                                       `((decode-sequence-of stream #',(decoder-name (cadr s-type))))))))
                                           (setf (,(accessor-name name s-name) choice) (dec stream))))))
                                (if tagged-alternatives
                                    (lambda (alternative)
                                      (destructuring-bind (s-name s-type &key tag) alternative
                                        (unless tag (error "Alternative ~S is missing ~S" alternative ':tag))
                                        (gen-alternative-decoder name s-name s-type tag)))
                                    (lambda (alternative)
                                      (destructuring-bind (s-name s-type) alternative
                                        ;; Note: We generate a *TYPE* and a GET-TYPE-FORM in each ASN.1 definition package.
                                        ;;       It should be the current package when defchoice is read and macroexpanded.
                                        ;;       Therefore we use (intern "GET-TYPE-FORM") to refer to the wanted function
                                        ;;       and *TYPE* table.
                                        (gen-alternative-decoder name s-name s-type (tag (cadr alternative)
                                                                                         (intern "GET-TYPE-FORM")))))))))
                       (if tagged-alternatives
                           `(let ((choice (,(%constructor-name name) nil nil))
                                  (tag    (decode-identifier stream)))
                              (decode-length stream)
                              (ecase tag
                                ,@(mapcar gen-alternative-clause alternatives))
                              choice)
                           `(let ((choice (,(%constructor-name name) nil nil))
                                  (tag    (decode-identifier stream)))
                              (flexi-streams:with-input-from-sequence (tag-stream (vector tag))
                                (let ((stream (make-concatenated-stream tag-stream stream)))
                                  (ecase tag
                                    ,@(mapcar gen-alternative-clause alternatives))))
                              choice)))))
             ,(if (and class tag)
                  `(decode-tagged-type stream #'dec)
                  `(dec stream)))))
       ',name)))


;; -----------------------------------

(defun tag (type-form get-type-form)
  (flet ((tag           (type-form) (tag type-form get-type-form))
         (get-type-form (type-name) (funcall get-type-form type-form))
         (primitive-tag (type-name)
           (case type-name
             ((:integer)                2)
             ((:bit-string)             3)
             ((:octet-string)           4)
             ((:null null)              5) ; TODO: Why does the parser generate null instead of :null?
             ((:object-identifier)      6)
             ((:sequence :sequence-of) 16)
             ((:set      :set-of)      17)
             ((:printable-string)      19)
             ((:t61-string)            20)
             ((:ia5-string)            22)
             ((:utc-time)              23)
             ((:choice)                (mapcar (function tag) (cddr type-form)))
             ((:tagged-type)           (destructuring-bind ((class tag) real-type &key &allow-other-keys) (cdr type-form)
                                         tag)))))
    (cond
      ((null type-form)            (error "Invalid type form ~S" type-form))
      ((symbolp type-form)         (or (primitive-tag type-form)
                                       (tag (get-type-form type-form))))
      ((consp type-form)           (or (primitive-tag (car type-form))
                                       (error "Unexpected type ~S" type-form)))
      (t                           (error "Invalid type form ~S" type-form)))))

(defun generate-type-registrations (assignments vtype stream)
  (terpri stream)
  (write-string "(eval-when (:compile-toplevel :load-toplevel :execute)" stream)
  (flet ((register-type (name type-form)
           (print `(setf (gethash ',name ,vtype) ',type-form) stream)))
    (dolist (assignment assignments)
      (destructuring-bind (name type-form) assignment
        (cond
          ((symbolp type-form)
           (register-type name type-form))
          ((consp type-form)
           (case (car type-form)
             ((:integer :bit-string :octet-string :sequence :choice :tagged-type)
              (register-type name type-form))
             ((:sequence-of)
              (let ((type-form (cadr type-form)))
                (unless (symbolp type-form)
                  (error "assigment ~A sequence-of must be a symbol" name)))
              (register-type name type-form))))))))
  (write-line ")" stream))

(defun gen (asn1 &optional (stream *standard-output*))
  (destructuring-bind (module-name assignments &key explicit implicit oid) (cdr asn1)
    (declare (ignore explicit implicit))
    (let ((*print-case* :downcase))
      (pprint `(defpackage ,(string-upcase module-name)
                 (:use #:cl #:asinine))
              stream)
      (terpri stream)
      (pprint `(in-package ,(string-upcase module-name))
              stream)
      (terpri stream)

      ;; start by defining the oid
      (pprint `(defparameter ,(alexandria:symbolicate '* module-name '-oid*) (list ,@(mapcar #'third oid)))
              stream)
      (terpri stream)

      ;; generate the types description used to decode untagged choices.
      (pprint `(defparameter *types* (make-hash-table))
              stream)
      (terpri stream)
      (pprint `(defun get-type-form (name) (gethash name *types*))
              stream)
      (terpri stream)
      (generate-type-registrations assignments '*types* stream)
      (terpri stream)

      ;; generate each assignment
      (dolist (assignment assignments)
        (destructuring-bind (name type-form) assignment
          (cond
            ((symbolp type-form)
             (pprint `(defun ,(encoder-name name) (stream value)
                        (,(encoder-name type-form) stream value))
                     stream)
             (pprint `(defun ,(decoder-name name) (stream)
                        (,(decoder-name type-form) stream))
                     stream))
            ((consp type-form)
             (case (car type-form)
               ((:integer)
                (pprint `(defun ,(encoder-name name) (stream value)
                           (encode-integer stream value))
                        stream)
                (pprint `(defun ,(decoder-name name) (stream)
                           (decode-integer stream))
                        stream))
               ((:bit-string)
                (pprint `(defun ,(encoder-name name) (stream value)
                                   (encode-bit-string stream value))
                                stream)
                (pprint `(defun ,(decoder-name name) (stream)
                           (decode-bit-string stream))
                        stream))
               ((:octet-string)
                (pprint `(defun ,(encoder-name name) (stream value)
                           (encode-octet-string stream value))
                        stream)
                (pprint `(defun ,(decoder-name name) (stream)
                           (decode-octet-string stream))
                        stream))
               ((:sequence-of)
                (let ((type-form (cadr type-form)))
                  (unless (symbolp type-form)
                    (error "assigment ~A sequence-of must be a symbol" name))
                  (pprint `(defun ,(encoder-name name) (stream value)
                             (encode-sequence-of stream #',(encoder-name type-form) value))
                          stream)
                  (pprint `(defun ,(decoder-name name) (stream)
                             (decode-sequence-of stream #',(decoder-name type-form)))
                          stream)))
               ((:sequence)
                (pprint `(defsequence ,name () ,@(cadr type-form)) stream))
               ((:choice)
                (pprint `(defchoice ,name () ,@(cadr type-form)) stream))
               ((:tagged-type)
                (destructuring-bind ((class tag) real-type &key &allow-other-keys) (cdr type-form)
                  (cond
                    ((symbolp real-type)
                     (pprint `(defun ,(encoder-name name) (stream value)
                                (encode-tagged-type stream #',(encoder-name real-type) value
                                                    :class ,class :tag ,tag))
                             stream)
                     (pprint `(defun ,(decoder-name name) (stream)
                                (decode-tagged-type stream #',(decoder-name real-type)))
                             stream))
                    ((eq (car real-type) :sequence)
                     (pprint `(defsequence ,name (,class ,tag) ,@(cadr real-type))
                             stream)))))
               ((:object-identifier)
                (let ((named-ints (cadr type-form)))
                  (pprint `(defparameter ,(alexandria:symbolicate '* name '-oid*)
                             (list ,@(mapcar #'third named-ints)))
                          stream)))
               (otherwise
                (warn "Not generating assignment for ~A" name))))
            (t (warn "Not generating assignment for ~A" name))))
        (terpri stream))))
  nil)
