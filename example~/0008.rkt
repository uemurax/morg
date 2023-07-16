#lang morg

@article[
  #:id (current-id)
  #:header @%{Definition}
  #:indexes @list[
    @index[#:key "thing"]{Thing}
  ]
  @paragraph{
    A @dfn{thing} consists of the following data.
    @unordered-list[
      @list-item{Something}
      @list-item{Some other thing}
    ]
    Moreover, it satisfies the following properties.
    @ordered-list[
      @list-item{Some axiom}
      @list-item{Another axiom}
    ]
  }
]
