#lang info

(define scribblings '(("scribblings/morg.scrbl" ())))
(define compile-omit-paths '("example/"))
(define raco-commands
  '(("morg" (submod morg/command main) "run MOrg" #f)))
