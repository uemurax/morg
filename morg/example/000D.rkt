#lang at-exp typed/racket

(require morg/markup
         morg/data/extension
         "math.rkt"
         "eq-reasoning.rkt")

(provide part:000D)

(define part:000D
  @article[
    #:id "000D"
    #:header "Proposition"
    @paragraph{
      @(math ("1" . = . "1"))
    }
    #:proof @proof[
      @paragraph{
        This is proved as follows.
        @disp{
          @extension[eq-reasoning-class
            @list[
              "" @math{1}
              @math{=} "{Definition}"
              "" @math{1 + 0}
              @math{=} "{Definition}"
              "" @math{1}
            ]
          ]
        }
      }
    ]
  ])

(module+ main
  (require morg/text)
  (display (->text part:000D)))
