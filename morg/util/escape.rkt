#lang typed/racket

(provide escape)

(module+ test
  (require typed/rackunit))

(define (escape [h : (HashTable String String)] [s : String]) : String
  (define pat
    (regexp
     (string-join (map regexp-quote (hash-keys h))
                  "|")))
  (regexp-replace* pat s
   (lambda ([x : String] . _rest)
     (hash-ref h x))))

(module+ test
  (define h
    (hash "\\" "\\\\"
          "#" "\\#"))
  (check-equal?
   (escape h "Hello \\world#\\#")
   "Hello \\\\world\\#\\\\\\#"))
