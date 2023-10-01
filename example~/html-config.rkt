#lang typed/racket

(require morg/html
         (prefix-in eq: morg/eq-reasoning/html))

(provide-config
 (compose-config
  eq:config-update
  (config-set-base-url "http://localhost")))
