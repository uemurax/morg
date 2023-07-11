#lang at-exp typed/racket

(require morg/markup)

(require "someone-2003-0000.rkt")

(provide part:0009)

(define part:0009
  @section[
    #:id "0009"
    #:title @%{Bibliography}
    part:someone-2003-0000
  ])

(module+ main
  (require morg/text)
  (display (->text part:0009)))
