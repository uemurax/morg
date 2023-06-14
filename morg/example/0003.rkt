#lang at-exp typed/racket

(require morg/markup)

(require "0004.rkt"
         "0006.rkt"
         "0008.rkt"
         "000B.rkt"
         "0007.rkt")

(provide part:0003)

(define part:0003
  @section[
    #:id "0003"
    #:title @%{Test section}
    @paragraph{
      This is a test section. See also @ref["0002"].
    }
    part:0004
    part:0006
    part:0008
    part:0007
    @paragraph{
      Please visit @href["https://example.com"]{this page}.
      Also check out @href["https://example.com"].
    }
    part:000B
  ])

(module+ main
  (require morg/text)
  (display (->text part:0003)))
