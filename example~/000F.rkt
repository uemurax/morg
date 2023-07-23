#lang morg

(require "lib/definition.rkt"
         "lib/notation-index.rkt"
         "math.rkt")

@definition[
  #:id (current-id)
  #:indexes @list[
    @notation-index[#:key "0"]{@(math "0")}
  ]
  @paragraph{
    We define @(math "0") to be the least natural number.
  }
]
