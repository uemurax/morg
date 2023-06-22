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
  (require morg/text
           morg/latex
           morg/html
           (prefix-in latex: morg/latex/config)
           (prefix-in latex:eq: (submod morg/eq-reasoning latex-config))
           (prefix-in html: morg/html/config)
           (prefix-in html:eq: (submod morg/eq-reasoning html-config)))
  (define-syntax (this-file stx)
    (with-syntax ([file (syntax-source stx)])
      #'file))
  (define this (cast (this-file) Path))
  (define this-dir (simplify-path (build-path this "..")))
  (define out-dir (build-path this-dir "_site"))
  (define latex-cfg
    (latex:eq:config-update latex:default-config))
  (define html-cfg
    (html:eq:config-update html:default-config))
  (define doc (include-part (submod "..")))
  (display "\nTEXT OUTPUT==============================\n")
  (display (->text doc))
  (display "\nPDF OUTPUT==============================\n")
  (->latex/publish #:config latex-cfg doc out-dir)
  (display "\nHTML OUTPUT==============================\n")
  (->html/publish #:config html-cfg doc out-dir))
