#lang morg

(require "math.rkt")

@article[
  #:id (current-id)
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
]
