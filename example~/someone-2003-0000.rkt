#lang morg

(require morg/bibliography)

@bibliography[
  #:id (current-id)
  @book[
    #:author @list[@%{Some One}]
    #:title @%{An introduction to something}
    #:date (date 2003)
    #:publisher @%{Some Publisher}
    #:url "https://example.com"
  ]
]
