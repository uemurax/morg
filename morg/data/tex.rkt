#lang typed/racket

(provide (struct-out text) Text
         (struct-out macro) Macro
         (struct-out group) Group
         (struct-out math) Math
         (except-out (struct-out sub-sup) sub-sup) SubSup
         (rename-out [make-sub-sup sub-sup])
         (struct-out text-tex) TextTeX
         (struct-out math-tex) MathTeX)

(module+ test
  (require typed/rackunit))

(struct text
  ([contents : String])
  #:transparent
  #:type-name Text)

(struct macro
  ([contents : String])
  #:transparent
  #:type-name Macro)

(struct (X) group
  ([contents : (Listof X)])
  #:transparent
  #:type-name Group)

(struct math
  ([contents : MathTeX])
  #:transparent
  #:type-name Math)

(struct sub-sup
  ([base : MathTeX]
   [sub : (Option MathTeX)]
   [sup : (Option MathTeX)])
  #:transparent
  #:type-name SubSup)

(define-type (TeXCommon X)
  (U Text
     Macro
     (Group X)))

(struct text-tex
  ([contents : (U (TeXCommon TextTeX)
                  Math)])
  #:transparent
  #:type-name TextTeX)

(struct math-tex
  ([contents : (U (TeXCommon MathTeX)
                  SubSup)])
  #:transparent
  #:type-name MathTeX)

(define (make-sub-sup [base : MathTeX] [sub : (Option MathTeX)] [sup : (Option MathTeX)]) : SubSup
  (define x (math-tex-contents base))
  (if (sub-sup? x)
      (if (or sub sup)
          (error "Double script.")
          x)
      (if (or sub sup)
          (sub-sup base sub sup)
          (error "Either sub or sup must be given."))))
