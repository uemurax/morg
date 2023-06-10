#lang at-exp typed/racket

(require morg/markup)

(require "0001.rkt"
         "0002.rkt")

(provide part:0000)

(define part:0000
  @section[
    #:id "0000"
    #:title @%{Introduction}
    @paragraph{
      This is a test document.
      See @ref["someone-2003-0000"] for more details.
    }
    #:subsections @list[
      part:0002
      part:0001
    ]
  ])

(module+ main
  (require morg/text)
  (display (->text part:0000)))
