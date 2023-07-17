#lang typed/racket

(require morg/latex
         (prefix-in eq: (submod morg/eq-reasoning latex-config)))

(provide-config
  (define cfg default-config)
  (eq:config-update cfg))
