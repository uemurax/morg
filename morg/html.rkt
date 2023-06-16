#lang typed/racket

(require "html/site.rkt"
         "html/config.rkt"
         "data/document.rkt")

(require/typed racket/file
  [make-temporary-directory (-> Path)])

(require/typed racket/base
  [copy-file (Path Path Boolean . -> . Void)])

(provide ->html
         ->html/publish)

(define (->html #:config [cfg : Config default-config]
                [doc : Document]) : Site
  (make-site cfg doc))

(define ((write-page [dir : Path])
         [url : String] [contents : String]) : Void
  (define file (build-path dir url))
  (call-with-output-file* file
   (lambda ([port : Output-Port])
     (write-string contents port)))
  (void))

(define (site-publish [dst-dir : Path] [s : Site]) : Void
  (define tmp-dir (make-temporary-directory))
  (hash-for-each s (write-page tmp-dir))
  (make-directory* dst-dir)
  (for-each
   (lambda ([f : Path])
     (copy-file (build-path tmp-dir f)
                (build-path dst-dir f)
                #t)
     (void))
   (directory-list tmp-dir))
  (void))

(define (->html/publish #:config [cfg : Config default-config]
                        [doc : Document]
                        [dst-dir : Path]) : Void
  (define s (->html #:config cfg doc))
  (site-publish dst-dir s))
