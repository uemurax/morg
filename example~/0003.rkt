#lang morg

@section[
  #:id (current-id)
  #:title @%{Test section}
  @paragraph{
    This is a test section. See also @ref["0002"].
  }
  (include-part "0004.rkt")
  (include-part "0006.rkt")
  (include-part "0008.rkt")
  (include-part "000F.rkt")
  (include-part "0007.rkt")
  @paragraph{
    Please visit @href["https://example.com"]{this page}.
    Also check out @href["https://example.com"].
  }
  (include-part "000B.rkt")
  (include-part "000C.rkt")
  (include-part "000D.rkt")
]
