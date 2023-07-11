#lang typed/racket

(require morg/html/config
         (prefix-in eq: (submod morg/eq-reasoning html-config)))

(provide-config
  (define cfg default-config)
  (eq:config-update cfg))
