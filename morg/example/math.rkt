#lang at-exp typed/racket

(require morg/math
         morg/markup/splice)

(provide + * =
         math)

(define cfg
  (struct-copy config default-config
   [levels '(* + =)]))

(define +
  (monoid #:level '+ @%{0} @%{+}))

(define *
  (monoid #:level '* @%{1} @%{@macro["times"]}))

(define =
  (binary #:level '= @%{=}))

(define math (make-math cfg))
