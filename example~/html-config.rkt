#lang typed/racket

(require morg/html
         (prefix-in eq: (submod morg/eq-reasoning html-config)))

(provide-config
  (define cfg default-config)
  (eq:config-update cfg))
