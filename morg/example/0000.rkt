#lang at-exp typed/racket

(require morg/markup)

(require "0001.rkt"
         "0002.rkt")

(provide part:0000)

(define part:0000
  @section[
    #:id "0000"
    #:title @~{Introduction}
    #:contents @list[
      @paragraph{
        This is a test document.
      }
    ]
    part:0002
    part:0001
  ])