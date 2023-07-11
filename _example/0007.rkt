#lang at-exp typed/racket

(require morg/markup
         "math.rkt")

(provide part:0007)

(define part:0007
  @article[
    #:id "0007"
    #:header @%{Fact}
    @paragraph{
      @math[
        (((@%{1} . + . @%{2})
          . * .
          (@%{3} . + . @%{4}))
         . = .
         @%{3})
      ].
    }
    @paragraph{
      @disp{
        @math[
          (((@%{1} . * . @%{2})
            . + .
            (@%{3} . * . @%{4}))
           . = .
           @%{5})
        ].
      }
    }
  ])

(module+ main
  (require morg/text)
  (display (->text part:0007)))
