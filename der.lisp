

(in-package #:asinine)


;; asn1:
;; FOOBAR ::= SEQUENCE { 
;;  A INTEGER
;;  B STRING
;;}
;; parsed to:
;;(:assignment foobar (:sequence ((a integer) (b string))))
;;


(defun generate-seq-struct (name seq-spec)
  `(defstruct ,name ,@(mapcar #'car (second seq-spec))))

(defun generate-seq-encoder (name seq-spec)
  `(defun ,(alexandria:symbolicate 'encode- name) (stream value)
     (encode-identifier stream :sequence)
     (let ((bytes (flexi-streams:with-output-to-sequence (s)
		    ,@(mapcar (lambda (slot)
				(let ((s-name (first slot))
				      (s-type (second slot)))
				  `(,(alexandria:symbolicate 'encode- s-type) s (,(alexandria:symbolicate name '- s-name) value))))
			      (second seq-spec)))))
       (encode-length stream (length bytes))
       (write-sequence bytes stream))))

(defun generate-seq-decoder (name seq-spec)
  `(defun ,(alexandria:symbolicate 'decode- name) (stream)
     (decode-identifier stream)
     (let ((bytes (nibbles:make-octet-vector (decode-length stream)))
	   (value (,(alexandria:symbolicate 'make- name))))
       (read-sequence bytes stream)
       (flexi-streams:with-input-from-sequence (s bytes)
	 (setf ,@(mapcan (lambda (slot)
			   (let ((s-name (first slot))
				 (s-type (second slot)))
			     (list `(,(alexandria:symbolicate name '- s-name) value)
				   `(,(alexandria:symbolicate 'decode- s-type) s))))
			 (second seq-spec))))
       value)))

;; ------------ want to generate -------------------

(defstruct foobar 
  a 
  b)

(defun encode-foobar (stream value)
  (encode-identifier stream :sequence)
  (let ((bytes (flexi-streams:with-output-to-sequence (s)
		 (encode-integer s (foobar-a value))
		 (encode-string s (foobar-b value)))))
    (encode-length stream (length bytes))
    (write-sequence bytes stream)))

(defun decode-foobar (stream)
  (decode-identifier stream)
  (let ((bytes (nibbles:make-octet-vector (decode-length stream)))
	(value (make-foobar)))
    (read-sequence bytes stream)
    (flexi-streams:with-input-from-sequence (s bytes)
      (setf (foobar-a value) (decode-integer s)
	    (foobar-b value) (decode-string s)))
    value))


