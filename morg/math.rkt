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
             [sub-sup% sub-sup]
             [paren% paren]
             [paren%/curried paren/curried]
             [dec-degree% dec-degree]
             [binary% binary]
             [big-op% big-op]
             [monoid% monoid])
 make-math)
