#lang morg

(require "0001.rkt"
         "0002.rkt")

@section[
  #:id (current-id)
  #:title @%{Introduction}
  @paragraph{
    This is a @emph{test} document.
    See @ref["someone-2003-0000"] for more details.
  }
  #:subsections @list[
    part:0002
    part:0001
  ]
]
