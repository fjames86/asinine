;;;; Copyright (c) Pascal J. Bourguignon 2016 <pjb@informatimago.com>
;;;; This code is licensed under the MIT license.

;;;; Testing defchoice.

(defun test-choice-alternative (object  value alternative type alternative-getter value-getter accessor bad-accessor)
  (check (typep  object type))
  (check (equalp       (funcall alternative-getter object) alternative))
  (check (equalp value (funcall value-getter object)))
  (check (equalp value (funcall accessor object)))
  (expect-error (funcall bad-accessor object)))

(defun test-integer-car (current value)
  (check (eql :primitive  (get-bsx-object-alternative (bsx-cons-head current))))
  (check (equalp :p-integer (get-bsx-primitive-alternative (get-bsx-object-value (bsx-cons-head current)))))
  (check (equalp value (get-bsx-primitive-value (get-bsx-object-value (bsx-cons-head current)))))
  (check (equalp value (bsx-primitive-p-integer (bsx-object-primitive (bsx-cons-head current))))))

(defun test-cons-cdr (current)
  (check (eql (get-bsx-object-value (bsx-cons-tail current))
              (bsx-object-cons      (bsx-cons-tail current))))
  (check (eql :cons (get-bsx-object-alternative (bsx-cons-tail current)))))

(defun test-null-cdr (current)
  (check (eql (get-bsx-object-value (bsx-cons-tail current))
              (bsx-object-null      (bsx-cons-tail current))))
  (let ((current (bsx-object-null (bsx-cons-tail current))))
    (check (eql 0 current))))

(testing

  ;; bsx-primitive

  (test-choice-alternative (make-p-integer-bsx-primitive 42) 42 :p-integer
                           'bsx-primitive
                           'get-bsx-primitive-alternative
                           'get-bsx-primitive-value
                           'bsx-primitive-p-integer
                           'bsx-primitive-p-bits)

  (test-choice-alternative (make-p-bytes-bsx-primitive #(1 2 3)) #(1 2 3) :p-bytes
                           'bsx-primitive
                           'get-bsx-primitive-alternative
                           'get-bsx-primitive-value
                           'bsx-primitive-p-bytes
                           'bsx-primitive-p-integer)

  (test-choice-alternative (make-p-bits-bsx-primitive #*11100100) #*11100100 :p-bits
                           'bsx-primitive
                           'get-bsx-primitive-alternative
                           'get-bsx-primitive-value
                           'bsx-primitive-p-bits
                           'bsx-primitive-p-bytes)


  ;; bsx-object

  (test-choice-alternative (make-null-bsx-object 0) 0 :null
                           'bsx-object
                           'get-bsx-object-alternative
                           'get-bsx-object-value
                           'bsx-object-null
                           'bsx-object-primitive)

  (let ((value (make-p-integer-bsx-primitive 42)))
    (test-choice-alternative (make-primitive-bsx-object value) value :primitive
                             'bsx-object
                             'get-bsx-object-alternative
                             'get-bsx-object-value
                             'bsx-object-primitive
                             'bsx-object-cons))

  (let ((value (make-bsx-cons :head (make-primitive-bsx-object (make-p-integer-bsx-primitive 42))
                              :tail (make-null-bsx-object 0))))
    (test-choice-alternative (make-cons-bsx-object value) value :cons
                             'bsx-object
                             'get-bsx-object-alternative
                             'get-bsx-object-value
                             'bsx-object-cons
                             'bsx-object-null))



  (let ((list (make-cons-bsx-object
               (make-bsx-cons :head (make-primitive-bsx-object (make-p-integer-bsx-primitive 1))
                              :tail (make-cons-bsx-object
                                     (make-bsx-cons :head (make-primitive-bsx-object (make-p-integer-bsx-primitive 2))
                                                    :tail (make-cons-bsx-object
                                                           (make-bsx-cons :head (make-primitive-bsx-object (make-p-integer-bsx-primitive 3))
                                                                          :tail (make-null-bsx-object 0)))))))))
    (print (get-bsx-object-alternative list))
    (check (eql :cons (get-bsx-object-alternative list)))
    (let ((current (bsx-object-cons list)))
      (check (eql (get-bsx-object-value list) current))
      (test-integer-car current 1)
      (test-cons-cdr current)
      (let ((current (bsx-object-cons (bsx-cons-tail current))))
        (test-integer-car current 2)
        (test-cons-cdr current)
        (let ((current (bsx-object-cons (bsx-cons-tail current))))
          (test-integer-car current 3)
          (test-null-cdr current))))))
