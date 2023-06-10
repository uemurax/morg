#lang at-exp typed/racket

(require morg/markup)

(provide part:0008)

(define part:0008
  @article[
    #:id "0008"
    #:header @%{Definition}
    @paragraph{
      A thing consists of the following data.
      @unordered-list[
        @list-item{Something}
        @list-item{Some other thing}
      ]
    }
  ])

(module+ main
  (require morg/text)
  (display (->text part:0008)))
