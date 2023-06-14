#lang at-exp typed/racket

(require morg/markup)

(provide part:0006)

(define part:0006
  @article[
    #:id "0006"
    #:header @%{Proposition}
    @paragraph{
      It is not true that it is true that it is not true that it is true that it is not true that it is true that false is false.
    }
    #:proof @proof[
      @paragraph{
        This essntially follows from @ref["0004"]. But we first have to observe that true is in fact false. Then, it follows that false is also false. Our claim is immediate from this.
      }
    ]
  ])

(module+ main
  (require morg/text)
  (display (->text part:0006)))
