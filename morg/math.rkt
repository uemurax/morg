#lang typed/racket

(require "math/markup.rkt"
         "math/format.rkt"
         "math/inline.rkt"
         "markup/tex.rkt")

(provide
 (struct-out user-config)
 default-config
 MathTeX+Like
 MathTeXAtom+Like
 (rename-out [macro% macro]
             [group% group]
             [argument% argument]
             [optional-argument% optional-argument]
             [paren% paren]
             [paren%/curried paren/curried]
             [dec-degree% dec-degree]
             [binary% binary]
             [big-op% big-op]
             [monoid% monoid])
 sub-sup _ ^
 make-math)

(define sub-sup (inst sub-sup% MathTeXAtom+Like MathTeX+Like))

(define (_ [base : MathTeXAtom+Like] [sub : MathTeX+Like])
  (sub-sup base #:_ sub))

(define (^ [base : MathTeXAtom+Like] [sup : MathTeX+Like])
  (sub-sup base #:^ sup))
