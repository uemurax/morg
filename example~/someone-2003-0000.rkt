#lang morg

(require (prefix-in bib: morg/bibliography))

@bib:bibliography[
  #:id (current-id)
  @bib:book[
    #:author @list[@%{Some One}]
    #:title @%{An introduction to something}
    #:date (date 2003)
    #:publisher @%{Some Publisher}
    #:url "https://example.com"
  ]
]
