#lang racket

(require syntax/strip-context
         (prefix-in at: scribble/reader)
         (submod "../markup/syntax.rkt" tools))

(provide
 (rename-out
  [morg-read read]
  [morg-read-syntax read-syntax]))

(define (morg-read in)
  (syntax->datum
   (morg-read-syntax #f in)))

(define (morg-read-syntax* src in acc)
  (define stx (at:read-syntax src in))
  (cond
   [(eof-object? stx) (reverse acc)]
   [else (morg-read-syntax* src in (list* stx acc))]))

(define (morg-read-syntax src in)
  (with-syntax ([(form ... result) (morg-read-syntax* src in (list))]
                [id (path->id src)]
                [provide-part-1 (gensym "provide-part-1")])
    (strip-context
     #'(module ignored typed/racket
         (require morg
                  morg/lang/id)

         (let ()
           (local-require (submod morg/lang/id parameter))
           (current-id id))

         form ...

         (require (rename-in (submod morg/markup/syntax internal)
                             [provide-part-0 provide-part-1]))

         (provide-part-1 result)

         (module+ main
           (require morg/text/preview)
           (preview))))))
