#lang at-exp typed/racket

(require "../data/document.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../data/node.rkt"
         "../data/index-table.rkt"
         "../markup/tex.rkt"
         "../markup/splice.rkt"
         "../util/list.rkt"
         "../text/date.rkt"
         "section.rkt"
         "inline.rkt"
         "block.rkt"
         "state.rkt"
         "config.rkt")

(provide document->latex)

(define (use-package [pkg : String])
  @macro%["usepackage" @argument%{@|pkg|}])

(define (pass-options-to-package [pkg : String]
                                 . [xs : Option *])
  @macro%["PassOptionsToPackage"
    @argument%{@(apply options% xs)}
    @argument%{@|pkg|}
  ])

(define (set-counter [cnt : String] [n : Integer])
  @macro%["setcounter" @argument%{@|cnt|} @argument%{@(number->string n)}])

(define ((document->latex [cfg : Config])
         [doc : Document]) : tex:TextTeX
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (define tbl (make-node-table main))
  (define untbl-1 (make-node-table front))
  (define untbl (make-node-table back #:init untbl-1))
  (define st
    (state cfg
            (make-index-table doc)
            tbl untbl))
  (define cls @argument%{@(config-class cfg)})
  (define cls-opt @optional-argument%{@(apply options% (config-class-options cfg))})
  (define f (section->latex st))
  (define g pure-inline->latex)
  (define h (block->latex st))
  (define pkgs (config-packages cfg))
  @text-tex%{
    @pass-options-to-package["hyperref"
     "pdfusetitle"
     '("pdfencoding" . "auto")
     "psdextra"
    ]
    @pass-options-to-package["url"
      "hyphens"
    ]
    @(apply %
      (map (lambda ([p : Package])
             (apply pass-options-to-package (package-name p) (package-options p)))
           pkgs))
    @macro%["documentclass" cls-opt cls]

    @(use-package "hyperref")
    @(use-package "xcolor")
    @(use-package "multicol")
    @(use-package "marginnote")
    @(apply %
      (map (lambda ([p : Package])
             (use-package (package-name p)))
           pkgs))

    @(set-counter "secnumdepth" -100)
    @(set-counter "tocdepth" 100)

    @macro%["title" @argument%{@(g (document-title doc))}]
    @macro%["author" (apply argument% (list-join-1 (map g (document-author doc)) @macro%["and"]))]
    @macro%["date" @argument%{@(date->text (document-date doc))}]

    @environment%["document"]{
      @(config-front-matter cfg)
      @macro%["maketitle"]
      @(h (document-contents doc))
      @macro%["tableofcontents"]
      @(apply % (map f front))
      @(config-main-matter cfg)
      @(apply % (map f main))
      @(config-back-matter cfg)
      @(apply % (map f back))
    }
  })
