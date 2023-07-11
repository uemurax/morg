#lang at-exp typed/racket

(require morg/markup)

(provide part:000B)

(define part:000B
  @article[
    #:id "000B"
    #:header @%{Remark}
    @paragraph{
      Here is a @code{code}.
    }
  ])

(module+ main
  (require morg/text)
  (display (->text part:000B)))
