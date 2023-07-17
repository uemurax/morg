#lang typed/racket

(require "html/config.rkt"
         "html/class/article.rkt"
         "html/class/block.rkt"
         "html/class/breadcrumb.rkt"
         "html/class/document.rkt"
         "html/class/document-toc.rkt"
         "html/class/d-pad.rkt"
         "html/class/id.rkt"
         "html/class/inline.rkt"
         "html/class/section.rkt"
         "html/class/toc.rkt")

(provide
 (struct-out config) Config
 provide-config
 default-config
 config-add-css
 (all-from-out
  "html/class/article.rkt"
  "html/class/block.rkt"
  "html/class/breadcrumb.rkt"
  "html/class/document.rkt"
  "html/class/document-toc.rkt"
  "html/class/d-pad.rkt"
  "html/class/id.rkt"
  "html/class/inline.rkt"
  "html/class/section.rkt"
  "html/class/toc.rkt"))
