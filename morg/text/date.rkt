#lang at-exp typed/racket

(require typed/racket/date
         "../markup/string.rkt")

(provide date->text)

(define (date->text [d : date]) : StringTree
  (parameterize ([date-display-format 'iso-8601])
    @string%{@(date->string d)}))
