#lang at-exp typed/racket

(require morg/markup)

(require "0004.rkt"
         "0006.rkt")

(provide part:0003)

(define part:0003
  @section[
    #:id "0003"
    #:title @~{Test section}
    @paragraph{
      This is a test section. See also @ref["0002"].
    }
    part:0004
    part:0006
  ])

(module+ main
  (require morg/text)
  (display (->text part:0003)))
