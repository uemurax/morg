#lang typed/racket

(require "convert.rkt"
         "../markup/syntax.rkt")

(provide preview)

(define-syntax-rule (preview)
  (display (->text (include-part (submod "..")))))
