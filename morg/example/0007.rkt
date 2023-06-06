#lang at-exp typed/racket

(require morg/markup)

(provide part:0007)

(define part:0007
  @article[
    #:id "0007"
    #:header @~{Fact}
    @paragraph{
      @math{1 + 1 = 3}.
    }
  ])

(module+ main
  (require morg/text)
  (display (->text part:0007)))
