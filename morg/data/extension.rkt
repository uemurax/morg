#lang typed/racket

(provide (struct-out ext-class) ExtClass
         extension extension-contents extension? Extension
         ExtHash ext-hash-ref
         empty-ext-hash ext-hash-insert
         extension-map)

(struct ext-class
  ()
  #:type-name ExtClass)

(struct (X) extension
  ([class : ExtClass]
   [contents : X])
  #:type-name Extension)

(struct (X) ext-hash
  ([contents : (HashTable ExtClass X)])
  #:type-name ExtHash)

(define #:forall (X Y)
        (ext-hash-ref [h : (ExtHash X)]
                      [y : (Extension Y)]
                      [def : (-> X)]) : X
  (hash-ref (ext-hash-contents h)
            (extension-class y)
            def))

(define #:forall (X) (empty-ext-hash) : (ExtHash X)
  ((inst ext-hash X) (hash)))

(define #:forall (X)
        (ext-hash-insert [h : (ExtHash X)]
                         [c : ExtClass]
                         [x : X]) : (ExtHash X)
  (define tbl (ext-hash-contents h))
  (cond
   [(hash-has-key? tbl c)
    (error "Key already exists.")]
   [else
    (ext-hash (hash-set tbl c x))]))

(define #:forall (X Y)
        ((extension-map [f : (X . -> . Y)])
         [x : (Extension X)]) : (Extension Y)
  (extension (extension-class x)
             (f (extension-contents x))))
