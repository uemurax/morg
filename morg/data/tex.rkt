#lang typed/racket

(require "splice.rkt")

(provide (struct-out text) Text
         (except-out (struct-out macro) macro) Macro
         (rename-out [make-macro macro])
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
  ([contents : X])
  #:transparent
  #:type-name Group)

(struct (X) math
  ([contents : X])
  #:transparent
  #:type-name Math)

(struct (X) sub-sup
  ([base : X]
   [sub : (Option X)]
   [sup : (Option X)])
  #:transparent
  #:type-name SubSup)

(define-type (TeXCommon X)
  (U (Splice X)
     Text
     Macro
     (Group X)))

(struct text-tex
  ([contents : (U (TeXCommon TextTeX)
                  (Math MathTeX))])
  #:transparent
  #:type-name TextTeX)

(struct math-tex
  ([contents : (U (TeXCommon MathTeX)
                  (SubSup MathTeX))])
  #:transparent
  #:type-name MathTeX)

(define (valid-macro? [s : String])
  (regexp-match-exact? #px"[[:alpha:]]+" s))

(module+ test
  (check-true (valid-macro? "test"))
  (check-true (valid-macro? "TEST"))
  (check-true (valid-macro? "ValidMacro"))
  (check-false (valid-macro? ""))
  (check-false (valid-macro? "Test1"))
  (check-false (valid-macro? "test-Macro")))

(define (make-macro [s : String]) : Macro
  (if (valid-macro? s)
      (macro s)
      (error (format "Invalid macro name: ~a" s))))

(define #:forall (X)
        (make-sub-sup [base : X] [sub : (Option X)] [sup : (Option X)]) : (SubSup X)
  (if (or sub sup)
      (sub-sup base sub sup)
      (error "Either sub or sup must be given.")))
