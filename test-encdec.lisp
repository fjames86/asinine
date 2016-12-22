;;;; Copyright (c) Pascal J. Bourguignon 2016 <pjb@informatimago.com>
;;;; This code is licensed under the MIT license.

;;;; Testing encoding and decoding of CHOICE.

(defun dump (seq &key readable)
  (format t (if readable
                "~&~{#x~2,'0X~^ ~}~%"
                "~&~{~2,'0X~^ ~}~%")
          (coerce seq 'list)))


(defun bsx-null ()
  (make-null-bsx-object 0))

(defun bsx-cons (a d)
  (make-cons-bsx-object (make-bsx-cons :head a :tail d)))

(defun bsx-list (&rest elements)
  (loop :for cell :=  (bsx-null) :then (bsx-cons element cell)
        :for element :in (reverse elements)
        :finally (return cell)))


(defun test-encode-decode (object encoder decoder)
  (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                    (funcall encoder stream object)))
    (check (equalp object (funcall decoder stream)))))


(testing
  (let* ((list (bsx-list
                (make-primitive-bsx-object (make-p-integer-bsx-primitive 42))
                (make-primitive-bsx-object (make-p-bits-bsx-primitive 424242))
                (make-primitive-bsx-object (make-p-bytes-bsx-primitive #(4 2 42 10 10 10)))))
         (pdu (make-bsx-pdu :version 1 :object list)))

    (test-encode-decode (make-p-integer-bsx-primitive 42)                                            'encode-bsx-primitive 'decode-bsx-primitive)
    (test-encode-decode (make-p-bits-bsx-primitive 424242)                                           'encode-bsx-primitive 'decode-bsx-primitive)
    (test-encode-decode (make-p-bytes-bsx-primitive #(4 2 42 10 10 10))                              'encode-bsx-primitive 'decode-bsx-primitive)

    (test-encode-decode (make-null-bsx-object 0)                                                     'encode-bsx-object 'decode-bsx-object)
    (test-encode-decode (make-primitive-bsx-object (make-p-integer-bsx-primitive 42))                'encode-bsx-object 'decode-bsx-object)
    (test-encode-decode (make-primitive-bsx-object (make-p-bits-bsx-primitive 424242))               'encode-bsx-object 'decode-bsx-object)
    (test-encode-decode (make-primitive-bsx-object (make-p-bytes-bsx-primitive #(4 2 42 10 10 10)))  'encode-bsx-object 'decode-bsx-object)
    (test-encode-decode list                                                                         'encode-bsx-object 'decode-bsx-object)

    (check (equalp (flexi-streams:with-output-to-sequence (stream)
                     (encode-bsx-pdu stream pdu))
                   *pdu-octets*))

    (check (equalp (flexi-streams:with-input-from-sequence (stream *pdu-octets*)
                     (decode-bsx-pdu stream))
                   pdu))

    (let* ((buffer (flexi-streams:with-output-to-sequence (stream)
                     (encode-bsx-pdu stream pdu)))
           (decoded (flexi-streams:with-input-from-sequence (stream buffer)
                      (decode-bsx-pdu  stream))))

      (dump buffer :readable t)
      (check (equalp pdu decoded)))))
