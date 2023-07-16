#lang morg

@section[
  #:id (current-id)
  #:title @%{Introduction}
  @paragraph{
    This is a @emph{test} document.
    See @ref["someone-2003-0000"] for more details.
  }
  #:subsections @list[
    (include-part "0002.rkt")
    (include-part "0001.rkt")
  ]
]
