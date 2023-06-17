#lang typed/racket

(provide (except-out (struct-out id) id) Id
         (rename-out [make-id id]))

(module+ test
  (require typed/rackunit))

(struct id
  ([contents : String])
  #:transparent
  #:type-name Id)

(define (valid-id? [x : String]) : Boolean
  (regexp-match-exact? #px"[[:alnum:]_-]+" x))

(module+ test
  (check-true (valid-id? "0123"))
  (check-true (valid-id? "abcd"))
  (check-true (valid-id? "ABCD"))
  (check-true (valid-id? "0Ab4"))
  (check-false (valid-id? ""))
  (check-true (valid-id? "xx_yy_Zz-02A5"))
  (check-true (valid-id? "0A-b4-3C")))

(define (make-id [x : String]) : Id
  (if (valid-id? x)
      (id x)
      (error (format "Invalid id: ~a" x))))
