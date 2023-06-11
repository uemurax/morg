#lang at-exp typed/racket

(require morg/math/markup
         morg/math/format
         morg/math/inline
         morg/markup/tex
         morg/markup/splice)

(provide + * =
         math)

(define cfg
  (struct-copy user-config default-config
   [levels '(* + =)]))

(define +
  (monoid% #:level '+ @%{0} @%{+}))

(define *
  (monoid% #:level '* @%{1} @%{@macro%["times"]}))

(define =
  (binary% #:level '= @%{=}))

(define math (make-math cfg))
