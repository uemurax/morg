#lang typed/racket

(require "../data/inline.rkt"
         "../data/splice.rkt"
         "../markup/string.rkt"
         "splice.rkt"
         "config.rkt")

(provide inline->text)

(: inline->text : (Config . -> . (Inline . -> . StringTree)))
(: text->text : (Config . -> . (Text . -> . StringTree)))

(define ((inline->text cfg) i)
  (define x (inline-contents i))
  (cond
   [(text? x) ((text->text cfg) x)]
   [(splice? x) ((splice->text (inline->text cfg)) x)]
   [else (error "Unimplemented.")]))

(define ((text->text _cfg) t)
  (string~ (text-contents t)))
