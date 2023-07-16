#lang morg

(require "lib/theorem.rkt"
         "lib/proof.rkt")

@theorem[
  #:id (current-id)
  @paragraph{
    True is true.
  }
  #:proof @proof[
    @paragraph{
      Left as an exercise.
    }
  ]
]
