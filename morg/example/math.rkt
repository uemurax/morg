#lang at-exp typed/racket

(require morg/math/markup
         morg/math/format
         morg/math/inline
         morg/markup/splice)

(provide + =
         math)

(define cfg
  (struct-copy user-config default-config
   [levels '(+ =)]))

(define +
  (binary% #:level '+ @%{+}))

(define =
  (binary% #:level '= @%{=}))

(define math (make-math cfg))
