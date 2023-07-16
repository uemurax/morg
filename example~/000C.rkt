#lang morg

@article[
  #:id (current-id)
  #:header @%{Theorem}
  @paragraph{
    The following is equivalent.
    @ordered-list[
      @list-item[#:id "0000"]{True.}
      @list-item[#:id "0001"]{Not false.}
    ]
  }
  #:proof @proof[
    @paragraph{
      The implication from @anchor-ref[#:anchor "0000" #:node "000C"] to @anchor-ref[#:anchor "0001" #:node "000C"] is easy.
    }
  ]
]
