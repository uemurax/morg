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
           morg/latex)
  (display "TEXT OUTPUT==============================")
  (display (->text part:index))
  (display "==============================TEXT OUTPUT")
  (display "LATEX OUTPUT==============================")
  (display (->latex part:index))
  (display "==============================LATEX OUTPUT"))
