#lang at-exp typed/racket

(require "../data/article.rkt"
         "../data/id.rkt"
         "../data/index.rkt"
         "inline.rkt"
         "block.rkt"
         "splice.rkt"
         "../util/option.rkt")

(provide article%/curried 
         article%
         proof%/curried
         proof%)

(define ((article%/curried [header : InlineLike])
         #:id [maybe-id : String]
         #:title [title : (Option InlineLike) #f]
         #:indexes [indexes : (Listof Index) '()]
         #:proof [proof : (Option Proof) #f]
         . [contents : BlockLike *]) : Article
  (article (id maybe-id)
           (inline% header)
           (option-map inline% title)
           indexes
           (apply block% contents)
           proof))

(define (article% #:id [maybe-id : String]
                  #:header [header : InlineLike]
                  #:title [title : (Option InlineLike) #f]
                  #:indexes [indexes : (Listof Index) '()]
                  #:proof [proof : (Option Proof) #f]
                  . [contents : BlockLike *]) : Article
  ((article%/curried header)
   #:id maybe-id
   #:title title
   #:indexes indexes
   #:proof proof
   (apply % contents)))

(define ((proof%/curried #:header [header : InlineLike @inline%{Proof}])
         . [contents : BlockLike *]) : Proof
  (proof (inline% header)
         (apply block% contents)))

(define (proof% #:header [header : InlineLike @inline%{Proof}]
                . [contents : BlockLike *]) : Proof
  (apply (proof%/curried #:header header) contents))
