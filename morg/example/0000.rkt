#lang at-exp typed/racket

(require morg/markup)

(require "0001.rkt"
         "0002.rkt")

(provide-part (id)
  @section[
    #:id id
    #:title @%{Introduction}
    @paragraph{
      This is a @emph{test} document.
      See @ref["someone-2003-0000"] for more details.
    }
    #:subsections @list[
      part:0002
      part:0001
    ]
  ])

(module+ main
  (require morg/text)
  (preview))
