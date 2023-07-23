#lang morg

@document[
  #:id (current-id)
  #:title @%{Test document}
  #:author @list[@%{Test Author}]
  #:contents @%[
    @paragraph{
      Abstract: This is a document.
    }
  ]
  #:front @list[
    (include-part "0005.rkt")
  ]
  (include-part "0000.rkt")
  (include-part "0003.rkt")
  #:back @list[
    (include-part "000A.rkt")
    (include-part "000E.rkt")
    (include-part "0009.rkt")
  ]
]
