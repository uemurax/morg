#lang at-exp typed/racket

(require morg/markup)

(provide part:000C)

(define part:000C
  @article[
    #:id "000C"
    #:header @%{Theorem}
    @paragraph{
      The following is equivalent.
      @ordered-list[
        @list-item[#:id "0000"]{True.}
        @list-item[#:id "0001"]{Not false.}
      ]
    }
    #:proof @proof[
      @paragraph{
        The implication from @anchor-ref[#:anchor "0000" #:node "000C"] to @anchor-ref[#:anchor "0001" #:node "000C"] is easy.
      }
    ]
  ])

(module+ main
  (require morg/text)
  (display (->text part:000C)))
