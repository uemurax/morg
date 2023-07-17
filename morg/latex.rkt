#lang typed/racket

(require "latex/config.rkt"
         "markup/tex.rkt")

(provide
 (struct-out config) Config
 (struct-out package) Package
 provide-config
 default-config
 TextTeXLike
 (rename-out
  [text-tex% text-tex]
  [argument% argument]
  [optional-argument% optional-argument]
  [star-argument% star-argument]
  [macro% macro]
  [macro-1% macro-1]
  [group% group]
  [special% special]
  [options% options]
  [environment% environment]))
