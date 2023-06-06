#lang at-exp typed/racket

(require morg/markup)

(provide part:0004)

(define part:0004
  @article[
    #:id "0004"
    #:header @~{Theorem}
    @paragraph{
      True is true.
    }
    #:proof @proof{
      @paragraph{
        Left as an exercise.
      }
    }
  ])
