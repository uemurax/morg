#lang typed/racket

(require "../class.rkt")

(provide document-class-name
         document-title-class-name
         document-address-class-name
         document-author-list-class-name
         document-author-class-name
         document-date-class-name)

(define document-class-name (class-name "document"))
(define document-title-class-name (class-name "document-title"))
(define document-address-class-name (class-name "document-address"))
(define document-author-list-class-name (class-name "document-author-list"))
(define document-author-class-name (class-name "document-author"))
(define document-date-class-name (class-name "document-date"))
