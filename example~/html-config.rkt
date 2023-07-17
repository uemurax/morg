#lang typed/racket

(require morg/html
         (prefix-in eq: morg/eq-reasoning/html))

(provide-config
  (define cfg default-config)
  (eq:config-update cfg))
