#lang typed/racket

(require "math/markup.rkt"
         "math/inline.rkt"
         "markup/tex.rkt")

(provide
 MathTeX+Like
 MathTeXAtom+Like
 (rename-out [paren% paren]
             [paren%/curried paren/curried]
             [dec-degree% dec-degree]
             [binary% binary]
             [unary% unary]
             [big-op% big-op]
             [delimiter% delimiter]
             [apply-with-parens% apply-with-parens]
             [sup-op% sup-op]
             [monoid% monoid])
 sub-sup _ ^
 macro
 macro-1
 group
 argument
 optional-argument
 math)

(define sub-sup (inst sub-sup% MathTeXAtom+Like MathTeX+Like))

(define (_ [base : MathTeXAtom+Like] [sub : MathTeX+Like])
  (sub-sup base #:_ sub))

(define (^ [base : MathTeXAtom+Like] [sup : MathTeX+Like])
  (sub-sup base #:^ sup))

(define macro (inst macro% MathTeX+Like))
(define macro-1 (inst macro-1% MathTeX+Like))
(define group (inst group% MathTeX+Like))
(define argument (inst argument% MathTeX+Like))
(define optional-argument (inst optional-argument% MathTeX+Like))
