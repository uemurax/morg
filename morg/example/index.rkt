#lang at-exp typed/racket

(require morg/markup)

(require "0000.rkt"
         "0003.rkt"
         "0005.rkt")

(provide part:index)

(define part:index
  @document[
    #:id "index"
    #:title @%{Test document}
    #:author @list[@%{Test Author}]
    #:front @list[
      part:0005
    ]
    part:0000
    part:0003
  ])

(module+ main
  (require morg/text
           morg/latex
           morg/html)
  (display "\nTEXT OUTPUT==============================\n")
  (display (->text part:index))
  (display "\n==============================TEXT OUTPUT\n")
  (display "\nLATEX OUTPUT==============================\n")
  (display (->latex part:index))
  (display "\n==============================LATEX OUTPUT\n")
  (display "\nHTML OUTPUT==============================\n")
  (write (->html part:index))
  (display "\n==============================HTML OUTPUT\n"))
