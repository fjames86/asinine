

(in-package #:asinine)

;; --------------------------------

(defun pack (encoder value)
  (flexi-streams:with-output-to-sequence (s)
    (funcall encoder s value)))

(defun unpack (decoder buffer)
  (flexi-streams:with-input-from-sequence (s buffer)
    (funcall decoder s)))

;; ----------------------------

;; Q: how to define various encoding rules? 

;; want various macros which are not tied directly to any specific encoding rule.
;; e.g.
;; (defoid)
;; (define-asn1-type)
;; (defsequence) 
;; (define-sequence-of)
;; (defchoice)
;; (defset)
;; (define-set-of)

;; A: we just hardcode der encoding rules and generate code that calls into this.
;; if we later want to support some other rule (e.g. cer) then we'd have to repeat the process.

