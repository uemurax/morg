#lang at-exp typed/racket

(require morg/markup)

(provide part:0001)

(define part:0001
  @section[
    #:id "0001"
    #:title @~{How to read this document}
    @paragraph{
      Don't read this document. Probably it is not good to refer to @ref["0005"].
    }
  ])
