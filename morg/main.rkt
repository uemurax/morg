#lang typed/racket

(require "markup/block.rkt"
         "markup/document.rkt"
         "markup/inline.rkt"
         "markup/section.rkt"
         "markup/date.rkt"
         "markup/index.rkt"
         "markup/splice.rkt"
         "markup/syntax.rkt")

(provide
 (rename-out [paragraph% paragraph]
             [document% document]
             [ref% ref]
             [href% href]
             [date% date]
             [emph% emph]
             [display% disp]
             [code% code]
             [dfn% dfn]
             [anchor-ref% anchor-ref]
             [index% index]
             [print-index% print-index]
             [section% section])
 anchor
 list-item
 ordered-list
 unordered-list
 (all-from-out "markup/splice.rkt")
 include-part)

(define anchor (inst anchor% PureInlineLike))
(define list-item (inst list-item% PureInlineLike InlineLike))
(define ordered-list (inst ordered-list% PureInlineLike InlineLike))
(define unordered-list (inst unordered-list% PureInlineLike InlineLike))
