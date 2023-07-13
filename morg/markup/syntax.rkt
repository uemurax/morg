#lang typed/racket

(require (for-syntax typed/racket))

(provide include-part
         dynamic-include-part
         provide-part)

(define-for-syntax part-name #'part)

(define-syntax (part-name stx)
  (with-syntax ([part part-name])
    #''part))

(define-syntax (include-part stx)
  (syntax-case stx ()
   [(_ path)
    (with-syntax ([part part-name])
      #'(let ()
          (local-require (rename-in path [part part:local]))
          part:local))]))

(define (dynamic-include-part [mod : Module-Path])
  (dynamic-require mod (part-name)))

(module tools typed/racket
  (provide path->id)

  (define (path->id [path : Path-String])
    (path->string
     (path-replace-extension
      (assert (file-name-from-path path) path?)
      ""))))

(require (for-syntax 'tools))

(define-syntax (provide-part-0 stx)
  (syntax-case stx ()
   [(_ form)
    (with-syntax ([part part-name])
      #'(begin
          (provide (rename-out [part:local part]))
          (define part:local
            form)))]))

(define-syntax (provide-part stx)
  (syntax-case stx ()
    [(_ (id-var) body ...)
     (with-syntax ([id (path->id (syntax-source stx))])
       #'(provide-part-0
          (let ([id-var id])
            body ...)))]))

(module* internal #f
  (provide provide-part-0))
