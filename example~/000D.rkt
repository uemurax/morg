#lang morg

(require morg/eq-reasoning
         "lib/proposition.rkt"
         "lib/proof.rkt"
         "math.rkt")

@proposition[
  #:id (current-id)
  @paragraph{
    @(math ("1" . = . "1"))
  }
  #:proof @proof[
    @paragraph{
      This is proved as follows.
      @disp{
        @eq-reasoning[
          @math{1}
          @math{=} "Definition"
          @math{1 + 0}
          @math{=} "Definition"
          @math{1}
        ]
      }
    }
  ]
]
