#lang at-exp typed/racket

(require morg/markup)

(require "0003.rkt"
         "0005.rkt"
         "000A.rkt"
         "0009.rkt")

(provide-part (id)
  @document[
    #:id id
    #:title @%{Test document}
    #:author @list[@%{Test Author}]
    #:contents @%[
      @paragraph{
        Abstract: This is a document.
      }
    ]
    #:front @list[
      part:0005
    ]
    (include-part "0000.rkt")
    part:0003
    #:back @list[
      part:000A
      part:0009
    ]
  ])

(module+ main
  (require morg/text)
  (preview))
