;;;; Copyright (c) Pascal J. Bourguignon 2016 <pjb@informatimago.com>
;;;; This code is licensed under the MIT license.

;;;; Testing defchoice.

(testing
  (check (typep (make-null-bsx-object 0) 'bsx-object))
  (check (typep (make-primitive-bsx-object #(1 2 3)) 'bsx-object))
  (check (typep (make-cons-bsx-object (make-bsx-cons :head (make-primitive-bsx-object #(1 2 3))
                                                     :tail (make-null-bsx-object 0)))
                'bsx-object))
  (expect-error (bsx-object-integer (make-null-bsx-object 0)))
  (expect-error (bsx-object-null (make-primitive-bsx-object #(1 2 3))))
  (check (equalp (bsx-object-primitive (make-primitive-bsx-object #(1 2 3))) #(1 2 3)))
  (check (eql    (get-bsx-object-alternative (make-primitive-bsx-object #(1 2 3))) :octet-string))
  (check (equalp (get-bsx-object-value (make-primitive-bsx-object #(1 2 3))) #(1 2 3)))

  (let ((list (make-cons-bsx-object
               (make-bsx-cons :head (make-primitive-bsx-object #(1 2 3))
                              :tail (make-cons-bsx-object
                                     (make-bsx-cons :head (make-primitive-bsx-object #(4 5 6))
                                                    :tail  (make-cons-bsx-object
                                                            (make-bsx-cons :head (make-primitive-bsx-object #(7 8 9))
                                                                           :tail (make-null-bsx-object 0)))))))))
    (print (get-bsx-object-alternative list))
    (check (eql :bsx-cons (get-bsx-object-alternative list)))
    (let ((current (bsx-object-cons list)))
      (check (eql (get-bsx-object-value list) current))
      (check (eql :octet-string  (get-bsx-object-alternative (bsx-cons-head current))))
      (check (equalp #(1 2 3) (get-bsx-object-value (bsx-cons-head current))))
      (check (equalp #(1 2 3) (bsx-object-primitive (bsx-cons-head current))))
      (check (eql :bsx-cons (get-bsx-object-alternative (bsx-cons-tail current))))
      (check (eql (get-bsx-object-value (bsx-cons-tail current))
                  (bsx-object-cons      (bsx-cons-tail current))))
      (let ((current (bsx-object-cons (bsx-cons-tail current))))
        (check (eql :octet-string  (get-bsx-object-alternative (bsx-cons-head current))))
        (check (equalp #(4 5 6) (get-bsx-object-value (bsx-cons-head current))))
        (check (equalp #(4 5 6) (bsx-object-primitive (bsx-cons-head current))))
        (check (eql :bsx-cons (get-bsx-object-alternative (bsx-cons-tail current))))
        (check (eql (get-bsx-object-value (bsx-cons-tail current))
                    (bsx-object-cons      (bsx-cons-tail current))))
        (let ((current (bsx-object-cons (bsx-cons-tail current))))
          (check (eql :octet-string  (get-bsx-object-alternative (bsx-cons-head current))))
          (check (equalp #(7 8 9) (get-bsx-object-value (bsx-cons-head current))))
          (check (equalp #(7 8 9) (bsx-object-primitive (bsx-cons-head current))))
          (check (eql :null     (get-bsx-object-alternative (bsx-cons-tail current))))
          (check (eql (get-bsx-object-value (bsx-cons-tail current))
                      (bsx-object-null      (bsx-cons-tail current))))
          (let ((current (bsx-object-null (bsx-cons-tail current))))
            (check (eql 0 current))))))))
