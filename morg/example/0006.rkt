#lang at-exp typed/racket

(require morg/markup)

(provide part:0006)

(define part:0006
  @article[
    #:id "0006"
    #:header @~{Proposition}
    @paragraph{
      False is false.
    }
    #:proof @proof[
      @paragraph{
        This follows from @ref["0004"].
      }
    ]
  ])

(module+ main
  (require morg/text)
  (display (->text part:0006)))
