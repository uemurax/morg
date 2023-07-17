#lang typed/racket

(require morg/latex
         (prefix-in eq: morg/eq-reasoning/latex))

(provide-config
  (define cfg default-config)
  (eq:config-update cfg))
