#lang at-exp typed/racket

(require "../data/document.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../data/node.rkt"
         "../markup/tex.rkt"
         "../markup/splice.rkt"
         "../util/list.rkt"
         "../text/date.rkt"
         "section.rkt"
         "inline.rkt"
         "block.rkt"
         "config.rkt")

(provide document->latex)

(define (use-package [pkg : String]) : tex:TextTeX
  @text-tex%{@macro%["usepackage" @argument%{@|pkg|}]})

(define ((document->latex [user-cfg : UserConfig])
         [doc : Document]) : tex:TextTeX
  (define front (document-front doc))
  (define main (document-main doc))
  (define back (document-back doc))
  (define tbl (make-node-table main))
  (define untbl-1 (make-node-table front))
  (define untbl (make-node-table back #:init untbl-1))
  (define cfg
    (config user-cfg
            tbl untbl))
  (define cls @argument%{@(user-config-class user-cfg)})
  (define cls-opt @optional-argument%{@(apply options% (user-config-class-options user-cfg))})
  (define f (section->latex cfg))
  (define g (inline->latex cfg))
  (define h (block->latex cfg))
  @text-tex%{
    @macro%["PassOptionsToPackage"
      @argument%{@options%["pdfusetitle" '("pdfencoding" . "auto") "psdextra"]}
      @argument%{hyperref}]
    @macro%["documentclass" cls-opt cls]

    @(use-package "hyperref")
    @(use-package "xcolor")

    @macro%["title" @argument%{@(g (document-title doc))}]
    @macro%["author" (apply argument% (list-join-1 (map g (document-author doc)) @macro%["and"]))]
    @macro%["date" @argument%{@(date->text (document-date doc))}]

    @environment%["document"]{
      @macro%["maketitle"]
      @(h (document-contents doc))
      @(apply % (map f front))
      @(apply % (map f main))
      @(apply % (map f back))
    }
  })
