#lang typed/racket

(require "../data/id.rkt"
         "config.rkt"
         "convert.rkt"
         "../data/document.rkt")

(require/typed racket/file
  [make-temporary-directory (-> Path)])

(require/typed racket/base
  [copy-file (Path Path Boolean . -> . Void)])

(provide ->latex/publish)

(define (latex->pdf/publish [dst-dir : Path] [i : Id] [s : String]) : Void
  (define cmd (find-executable-path "latexmk"))
  (unless cmd
    (error "latexmk not found."))
  (define name-base (id-contents i))
  (define tmp-dir
    (make-temporary-directory))
  (define tex-file
    (build-path tmp-dir (path-add-extension name-base ".tex")))
  (call-with-output-file* tex-file
   (lambda ([port : Output-Port])
     (write-string s port)))
  (define latex-res
    (parameterize ([current-directory tmp-dir])
      (system* cmd "-pdf" "-lualatex" tex-file
               #:set-pwd? #t)))
  (unless latex-res
    (error "latexmk failed."))
  (make-directory* dst-dir)
  (define name-pdf (path-add-extension name-base ".pdf"))
  (copy-file (build-path tmp-dir name-pdf)
             (build-path dst-dir name-pdf)
             #t)
  (void))

(define (->latex/publish #:config [cfg : Config default-config]
                         [doc : (U Document)]
                         [dst-dir : Path]) : Void
  (define s (->latex #:config cfg doc))
  (latex->pdf/publish dst-dir (document-id doc) s))
