#lang typed/racket

(require morg/html
         (prefix-in eq: morg/eq-reasoning/html))

(provide-config
  (define cfg default-config)
  (config-set-base-url
   (eq:config-update cfg)
   "http://localhost"))
