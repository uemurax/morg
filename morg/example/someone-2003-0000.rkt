#lang at-exp typed/racket

(require morg/markup
         (prefix-in b: morg/bibliography))

(provide part:someone-2003-0000)

(define part:someone-2003-0000
  @b:bib[
    "someone-2003-0000"
    @b:book[
      #:author @list[@%{Some One}]
      #:title @%{An introduction to something}
      #:date (date 2003)
      #:publisher @%{Some Publisher}
      #:url "https://example.com"
    ]
  ])

(module+ main
  (require morg/text)
  (display (->text part:someone-2003-0000)))
