#lang at-exp typed/racket

(require morg/markup)

(provide part:0008)

(define part:0008
  @article[
    #:id "0008"
    #:header @%{Definition}
    #:indexes @list[
      @index[#:key "thing"]{Thing}
    ]
    @paragraph{
      A @dfn{thing} consists of the following data.
      @unordered-list[
        @list-item{Something}
        @list-item{Some other thing}
      ]
      Moreover, it satisfies the following properties.
      @ordered-list[
        @list-item{Some axiom}
        @list-item{Another axiom}
      ]
    }
  ])

(module+ main
  (require morg/text)
  (display (->text part:0008)))
