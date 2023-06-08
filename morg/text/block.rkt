#lang at-exp typed/racket

(require "../data/block.rkt"
         "../data/splice.rkt"
         "../markup/string.rkt"
         "splice.rkt"
         "inline.rkt"
         "config.rkt")

(provide block->text)

(: block->text : (Config . -> . (Block . -> . StringTree)))
(: paragraph->text : (Config . -> . (Paragraph . -> . StringTree)))

(define ((block->text cfg) b)
  (define x (block-contents b))
  (cond
   [(paragraph? x)
    ((paragraph->text cfg) x)]
   [(splice? x)
    ((splice->text (block->text cfg)) x)]
   [else (error "Unimplemented.")]))

(define ((paragraph->text cfg) p)
  @string%{

    @((inline->text cfg) (paragraph-contents p))

  })
