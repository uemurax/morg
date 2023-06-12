#lang at-exp typed/racket

(require "../data/article.rkt"
         "../data/id.rkt"
         "../data/index.rkt"
         "inline.rkt"
         "block.rkt"
         "../util/option.rkt")

(provide article% proof%)

(define (article% #:id [maybe-id : String]
                  #:header [header : InlineLike]
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

(define (proof% #:header [header : InlineLike @inline%{Proof}]
                . [contents : BlockLike *]) : Proof
  (proof (inline% header)
         (apply block% contents)))
