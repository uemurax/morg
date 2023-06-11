#lang typed/racket

(require "markup/article.rkt"
         "markup/block.rkt"
         "markup/document.rkt"
         "markup/inline.rkt"
         "markup/section.rkt"
         "markup/date.rkt"
         "markup/splice.rkt")

(provide
 (rename-out [article% article]
             [proof% proof]
             [paragraph% paragraph]
             [document% document]
             [ref% ref]
             [list-item% list-item]
             [unordered-list% unordered-list]
             [href% href]
             [date% date]
             [emph% emph]
             [section% section])
 (all-from-out "markup/splice.rkt"))
