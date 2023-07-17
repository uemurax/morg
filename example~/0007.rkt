#lang morg

(require "math.rkt"
         "lib/fact.rkt")

@fact[
  #:id (current-id)
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
]
