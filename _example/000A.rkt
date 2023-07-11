#lang at-exp typed/racket

(require morg/markup)

(provide part:000A)

(define part:000A
  @section[
    #:id "000A"
    #:title @%{Index}
    @print-index[]
  ])

(module+ main
  (require morg/text)
  (display (->text part:000A)))
