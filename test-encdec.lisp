;;;; Copyright (c) Pascal J. Bourguignon 2016 <pjb@informatimago.com>
;;;; This code is licensed under the MIT license.

;;;; Testing encoding and decoding of CHOICE.

(defun dump (seq &key readable)
  (format t (if readable
                "~&~{#x~2,'0X~^ ~}~%"
                "~&~{~2,'0X~^ ~}~%")
          (coerce seq 'list)))

(testing
  (let ((pdu (make-bsx-pdu :version 1
                           :object (make-cons-bsx-object
                                    (make-bsx-cons :head (make-primitive-bsx-object #(1 2 3))
                                                   :tail  (make-cons-bsx-object
                                                           (make-bsx-cons :head (make-primitive-bsx-object #(4 5 6))
                                                                          :tail (make-null-bsx-object 0))))))))
    (check (equalp (flexi-streams:with-output-to-sequence (stream)
                     (encode-bsx-pdu stream pdu))
                   *pdu-octets*)))


  (flexi-streams:with-output-to-sequence (stream)
    (encode-bsx-object stream  (make-null-bsx-object 0)))

  (let ((object (make-null-bsx-object 0)))
    (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                      (encode-bsx-object stream object)))
      (check (equalp object (decode-bsx-object stream)))))


  (let ((object (make-primitive-bsx-object #(4 5 6))))
    (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                      (encode-bsx-object stream object)))
      (check (equalp object (decode-bsx-object stream)))))


  (let ((cons  (make-bsx-cons :head (make-primitive-bsx-object #(4 5 6))
                              :tail (make-null-bsx-object 0))))
    (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                      (encode-bsx-cons stream cons)))
      (check (equalp cons (decode-bsx-cons stream)))))


  (let ((object (make-cons-bsx-object
                 (make-bsx-cons :head (make-primitive-bsx-object #(4 5 6))
                                :tail (make-null-bsx-object 0)))))
    (flexi-streams:with-input-from-sequence (stream (flexi-streams:with-output-to-sequence (stream)
                                                      (encode-bsx-object stream
                                                                         object)))
      (check (equalp object (decode-bsx-object stream)))))


  (let ((pdu (make-bsx-pdu :version 1
                           :object (make-cons-bsx-object
                                    (make-bsx-cons :head (make-primitive-bsx-object #(1 2 3))
                                                   :tail (make-cons-bsx-object
                                                          (make-bsx-cons :head (make-primitive-bsx-object #(4 5 6))
                                                                         :tail (make-null-bsx-object 0))))))))
    (check (equalp pdu
                   (flexi-streams:with-input-from-sequence (stream *pdu-octets*)
                     (decode-bsx-pdu stream)))))


  (let* ((pdu (make-bsx-pdu :version 1
                            :object (make-cons-bsx-object
                                     (make-bsx-cons :head (make-primitive-bsx-object #(1 2 3))
                                                    :tail (make-cons-bsx-object
                                                           (make-bsx-cons :head (make-primitive-bsx-object #(4 5 6))
                                                                          :tail (make-null-bsx-object 0)))))))
         (buffer (flexi-streams:with-output-to-sequence (stream)
                   (encode-bsx-pdu stream pdu)))
         (decoded (flexi-streams:with-input-from-sequence (stream buffer)
                    (decode-bsx-pdu  stream))))
    (dump buffer :readable t)
    (check (equalp pdu decoded))))

