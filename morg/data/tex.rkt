#lang typed/racket

(require "splice.rkt"
         "../util/option.rkt")

(provide (struct-out text) Text
         (struct-out argument) Argument
         (struct-out macro) Macro
         (struct-out group) Group
         (struct-out atom) Atom
         (struct-out math) Math
         (except-out (struct-out sub-sup) sub-sup) SubSup
         (rename-out [make-sub-sup sub-sup])
         (struct-out text-tex) TextTeX
         (struct-out math-tex) MathTeX
         group-map
         argument-map
         atom-map
         sub-sup-map
         macro-map)

(struct text
  ([contents : String])
  #:transparent
  #:type-name Text)

(struct (X) argument
  ([contents : X]
   [parentheses : (Pairof String String)])
  #:transparent
  #:type-name Argument)

(struct (X) macro
  ([head : String]
   [arguments : (Listof (Argument X))])
  #:transparent
  #:type-name Macro)

(struct (X) group
  ([contents : X])
  #:transparent
  #:type-name Group)

(struct (X) atom
  ([contents : (U Text (Macro X) (Group X))])
  #:transparent
  #:type-name Atom)

(struct math
  ([contents : MathTeX])
  #:transparent
  #:type-name Math)

(struct (A X) sub-sup
  ([base : A]
   [sub : (Option X)]
   [sup : (Option X)])
  #:transparent
  #:type-name SubSup)

(struct text-tex
  ([contents : (U (Atom TextTeX)
                  (Splice TextTeX)
                  Math)])
  #:transparent
  #:type-name TextTeX)

(struct math-tex
  ([contents : (U (Atom MathTeX)
                  (Splice MathTeX)
                  (SubSup (Atom MathTeX) MathTeX))])
  #:transparent
  #:type-name MathTeX)

(define #:forall (A X)
        (make-sub-sup [base : A] [sub : (Option X)] [sup : (Option X)]) : (SubSup A X)
  (if (or sub sup)
      (sub-sup base sub sup)
      (error "Either sub or sup must be given.")))

(define #:forall (X Y)
        ((group-map [f : (X . -> . Y)]) [x : (Group X)]) : (Group Y)
  (group (f (group-contents x))))

(define #:forall (X Y)
        ((argument-map [f : (X . -> . Y)]) [x : (Argument X)]) : (Argument Y)
  (argument (f (argument-contents x))
            (argument-parentheses x)))

(define #:forall (X Y)
        ((macro-map [f : (X . -> . Y)]) [x : (Macro X)]) : (Macro Y)
  (macro (macro-head x)
         (map (argument-map f) (macro-arguments x))))

(define #:forall (X Y)
        ((atom-map [f : (X . -> . Y)]) [x : (Atom X)]) : (Atom Y)
  (define a (atom-contents x))
  (define b
    (cond
     [(text? a) a]
     [(macro? a) ((macro-map f) a)]
     [(group? a) ((group-map f) a)]))
  (atom b))

(define #:forall (A B X Y)
        ((sub-sup-map [g : (A . -> . B)] [f : (X . -> . Y)])
         [x : (SubSup A X)]) : (SubSup B Y)
  (sub-sup
   (g (sub-sup-base x))
   (option-map f (sub-sup-sub x))
   (option-map f (sub-sup-sup x))))
