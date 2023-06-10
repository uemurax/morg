#lang at-exp typed/racket

(require typed/racket/date
         "../markup/string.rkt"
         "config.rkt")

(provide date->text)

(define ((date->text [_cfg : Config])
         [d : date]) : StringTree
  (parameterize ([date-display-format 'iso-8601])
    @string%{@(date->string d)}))
