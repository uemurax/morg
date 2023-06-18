#lang at-exp typed/racket

(require "../data/id.rkt"
         (prefix-in tex: "../data/tex.rkt")
         "../text/id.rkt"
         "../markup/tex.rkt"
         "../markup/splice.rkt")

(provide id->latex
         anchor-id->latex
         id->latex/margin
         anchor-id->hypertarget
         anchor-id->hyperlink
         id->hypertarget
         id->hyperlink)

(define (id-style [a : TextTeXLike])
  @macro%["textcolor" @argument%{gray} @argument%{@macro%["normalsize"]@macro%["texttt" @argument%{@|a|}]}])

(define (id->latex [i : Id])
  (id-style (id->text i)))

(define (anchor-id->latex [node : Id] [anchor : Id])
  (id-style (anchor-id->text node anchor)))

(define (id->latex/margin [i : Id])
  @macro%["marginnote" @argument%{@(id->latex i)}])

(define (id->label [i : Id])
  @text-tex%{morg-generated-@(id-contents i)})

(define (hypertarget [label : TextTeXLike] [text : TextTeXLike])
  @macro%["hypertarget" @argument%{@|label|} @argument%{@|text|}])

(define (hyperlink [label : TextTeXLike] [text : TextTeXLike])
  @macro%["hyperlink" @argument%{@|label|} @argument%{@|text|}])

(define (id->hypertarget [i : Id])
  (hypertarget (id->label i) ""))

(define (id->hyperlink [i : Id] [x : TextTeXLike])
  (hyperlink (id->label i) x))

(define (anchor-id->label [node : Id] [anchor : Id])
  @text-tex%{morg-generated-anchor-@(id-contents node)---@(id-contents anchor)})

(define (anchor-id->hypertarget [node : Id] [anchor : Id] [label : TextTeXLike])
  (hypertarget (anchor-id->label node anchor) label))

(define (anchor-id->hyperlink [node : Id] [anchor : Id] [label : TextTeXLike])
  (hyperlink (anchor-id->label node anchor) label))
