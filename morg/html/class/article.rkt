#lang typed/racket

(require "../class.rkt")

(provide statement-class-name
         statement-header-class-name
         statement-header-header-class-name
         statement-header-title-class-name
         statement-body-class-name
         proof-class-name
         proof-header-class-name
         proof-body-class-name
         article-class-name)

(define statement-class-name (class-name "statement"))
(define statement-header-class-name (class-name "statement-header"))
(define statement-header-header-class-name (class-name "statement-header-header"))
(define statement-header-title-class-name (class-name "statement-header-title"))
(define statement-body-class-name (class-name "statement-body"))
(define proof-class-name (class-name "proof"))
(define proof-header-class-name (class-name "proof-header"))
(define proof-body-class-name (class-name "proof-body"))
(define article-class-name (class-name "article"))
