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

(define ((article%/curried [header : PureInlineLike])
         #:id [maybe-id : String]
         #:title [title : (Option PureInlineLike) #f]
         #:indexes [indexes : (Listof Index) '()]
         #:proof [proof : (Option Proof) #f]
         . [contents : BlockLike *]) : Article
  (article (id maybe-id)
           (pure-inline% header)
           (option-map pure-inline% title)
           indexes
           (apply block% contents)
           proof))

(define (article% #:id [maybe-id : String]
                  #:header [header : PureInlineLike]
                  #:title [title : (Option PureInlineLike) #f]
                  #:indexes [indexes : (Listof Index) '()]
                  #:proof [proof : (Option Proof) #f]
                  . [contents : BlockLike *]) : Article
  ((article%/curried header)
   #:id maybe-id
   #:title title
   #:indexes indexes
   #:proof proof
   (apply % contents)))

(define ((proof%/curried #:header [header : PureInlineLike "Proof"])
         . [contents : BlockLike *]) : Proof
  (proof (pure-inline% header)
         (apply block% contents)))

(define (proof% #:header [header : PureInlineLike "Proof"]
                . [contents : BlockLike *]) : Proof
  (apply (proof%/curried #:header header) contents))
