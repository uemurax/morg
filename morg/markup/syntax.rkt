#lang racket

(require (for-syntax racket))

(provide include-part
         provide-part)

(define-for-syntax part-name #'part)

(define-syntax (include-part stx)
  (syntax-case stx ()
   [(_ id)
    (with-syntax ([part part-name]
                  [path (let ([id-1 (syntax->datum #'id)])
                          (if (eq? id-1 #f)
                              ".."
                              (symbol->string id-1)))])
      #'(let ()
          (local-require (rename-in path [part part:local]))
          part:local))]))

(define-for-syntax (path->id path)
  (path->string
   (path-replace-extension
    (file-name-from-path path)
    "")))

(define-syntax (provide-part stx)
  (syntax-case stx ()
    [(_ (id-var) body ...)
     (with-syntax ([part part-name]
                   [id (path->id (syntax-source stx))])
       #'(begin
           (provide (rename-out [part:local part]))
           (define part:local
             (let ([id-var id])
               body ...))))]))
