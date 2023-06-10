#lang at-exp typed/racket

(require morg/markup)

(require "0000.rkt"
         "0003.rkt"
         "0005.rkt"
         "0009.rkt")

(provide part:index)

(define part:index
  @document[
    #:id "index"
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
    part:0000
    part:0003
    #:back @list[
      part:0009
    ]
  ])

(module+ main
  (require morg/text
           morg/latex
           morg/html)
  (define-syntax (this-file stx)
    (with-syntax ([file (syntax-source stx)])
      #'file))
  (define this (cast (this-file) Path))
  (define this-dir (simplify-path (build-path this "..")))
  (define out-dir (build-path this-dir "_site"))
  (display "\nTEXT OUTPUT==============================\n")
  (display (->text part:index))
  (display "\nPDF OUTPUT==============================\n")
  (->latex/publish part:index out-dir)
  (display "\nHTML OUTPUT==============================\n")
  (->html/publish part:index out-dir))
