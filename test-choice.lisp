(in-package #:asinine)

;; Testing defchoice.

(defmacro expect-error (expression)
  `(handler-case
       ,expression
     (:no-error (value)
       (error "Expected an error for ~S but got result ~S"
              ',expression value))
     (error ()
       :success)))

(defsequence bsx-cons () (head bsx-object) (tail bsx-object))
(defchoice bsx-object () (null null) (integer integer) (cons bsx-cons))

(assert (typep (make-null-bsx-object 0) 'bsx-object))
(assert (typep (make-integer-bsx-object 42) 'bsx-object))
(assert (typep (make-cons-bsx-object (make-bsx-cons :head (make-integer-bsx-object 42)
                                                    :tail (make-null-bsx-object 0)))
               'bsx-object))
(expect-error (bsx-object-integer (make-null-bsx-object 0)))
(expect-error (bsx-object-null (make-integer-bsx-object 42)))
(assert (eql (bsx-object-integer (make-integer-bsx-object 42)) 42))
(assert (eql (get-bsx-object-alternative (make-integer-bsx-object 42)) :integer))
(assert (eql (get-bsx-object-value (make-integer-bsx-object 42)) 42))

(let ((list (make-cons-bsx-object
             (make-bsx-cons :head (make-integer-bsx-object 1)
                            :tail (make-cons-bsx-object
                                   (make-bsx-cons :head (make-integer-bsx-object 2)
                                                  :tail  (make-cons-bsx-object
                                                          (make-bsx-cons :head (make-integer-bsx-object 3)
                                                                         :tail (make-null-bsx-object 0)))))))))
  (print (get-bsx-object-alternative list))
  (assert (eql :bsx-cons (get-bsx-object-alternative list)))
  (let ((current (bsx-object-cons list)))
    (assert (eql (get-bsx-object-value list) current))
    (assert (eql :integer  (get-bsx-object-alternative (bsx-cons-head current))))
    (assert (eql 1               (get-bsx-object-value (bsx-cons-head current))))
    (assert (eql 1                 (bsx-object-integer (bsx-cons-head current))))
    (assert (eql :bsx-cons (get-bsx-object-alternative (bsx-cons-tail current))))
    (assert (eql (get-bsx-object-value (bsx-cons-tail current))
                 (bsx-object-cons      (bsx-cons-tail current))))
    (let ((current (bsx-object-cons (bsx-cons-tail current))))
      (assert (eql :integer  (get-bsx-object-alternative (bsx-cons-head current))))
      (assert (eql 2               (get-bsx-object-value (bsx-cons-head current))))
      (assert (eql 2                 (bsx-object-integer (bsx-cons-head current))))
      (assert (eql :bsx-cons (get-bsx-object-alternative (bsx-cons-tail current))))
      (assert (eql (get-bsx-object-value (bsx-cons-tail current))
                   (bsx-object-cons      (bsx-cons-tail current))))
      (let ((current (bsx-object-cons (bsx-cons-tail current))))
        (assert (eql :integer  (get-bsx-object-alternative (bsx-cons-head current))))
        (assert (eql 3               (get-bsx-object-value (bsx-cons-head current))))
        (assert (eql 3                 (bsx-object-integer (bsx-cons-head current))))
        (assert (eql :null     (get-bsx-object-alternative (bsx-cons-tail current))))
        (assert (eql (get-bsx-object-value (bsx-cons-tail current))
                     (bsx-object-null      (bsx-cons-tail current))))
        (let ((current (bsx-object-null (bsx-cons-tail current))))
          (assert (eql 0 current)))))))
