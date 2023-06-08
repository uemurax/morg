#lang typed/racket

(provide (struct-out config) Config
         (struct-out user-config) UserConfig)

(struct config
  ([user-config : UserConfig])
  #:transparent
  #:type-name Config)

(struct user-config
  ()
  #:transparent
  #:type-name UserConfig)
