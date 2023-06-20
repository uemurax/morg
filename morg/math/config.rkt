#lang typed/racket

(provide (struct-out config) Config
         default-config)

(struct config
  ([levels : (Listof Symbol)])
  #:transparent
  #:type-name Config)

(define default-config
  (config
   '(+ * generic-bin generic-rel comma)))
