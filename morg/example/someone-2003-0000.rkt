#lang at-exp typed/racket

(require morg/markup
         morg/bibliography)

(provide part:someone-2003-0000)

(define part:someone-2003-0000
  @bib[
    "someone-2003-0000"
    @book[
      #:author @list[@%{Some One}]
      #:title @%{An introduction to something}
      #:date (date #:year 2003)
      #:publisher @%{Some Publisher}
      #:url "https://example.com"
    ]
  ])

(module+ main
  (require morg/text)
  (display (->text part:someone-2003-0000)))
