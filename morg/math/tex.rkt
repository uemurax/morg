#lang typed/racket

(require "../math.rkt"
         "../markup/splice.rkt")

(require (for-syntax racket
                     syntax/parse))

(define-for-syntax (make-provide-syntax stx fun)
  (syntax-parse stx
   [(_ id:identifier)
    (with-syntax ([str (symbol->string (syntax->datum #'id))]
                  [tmp (gensym "tmp")]
                  [f fun])
      #'(begin
          (provide (rename-out [tmp id]))
          (define tmp (f str))))]))

(define-syntax (provide-macro stx)
  (make-provide-syntax stx #'macro))

(define-syntax-rule (provide-macros id ...)
  (begin (provide-macro id) ...))

(define-syntax (provide-macro-1 stx)
  (make-provide-syntax stx #'macro-1))

(define-syntax-rule (provide-macros-1 id ...)
  (begin (provide-macro-1 id) ...))

;;; Macros supported by both LaTeX and KaTeX

(provide-macros-1
 acute
 bar
 breve
 check
 dot
 ddot
 grave
 hat
 widehat
 tilde
 widetilde
 vec
 overleftarrow
 mathring
 overrightarrow
 overbrace
 underbrace
 underbar)

(provide-macros
 \{
 \}
 \|
 lbrack
 rbrack
 lbrace
 rbrace
 langle
 rangle
 vert
 Vert
 lceil
 rceil
 lfloor
 rfloor
 lmoustache
 rmoustache
 lgroup
 rgroup
 uparrow
 downarrow
 updownarrow
 Uparrow
 Downarrow
 Updownarrow
 backslash)

(define ((make-delimit-cmd [a : String])
         [x : MathTeX+Like])
  (% (macro a) x))

(define-syntax (provide-delimit-macro stx)
  (make-provide-syntax stx #'make-delimit-cmd))

(define-syntax-rule (provide-delimit-macros id ...)
  (begin (provide-delimit-macro id) ...))

(provide-delimit-macros
 left
 right
 big
 Big
 bigg
 Bigg
 bigl
 bigm
 bigr
 Bigl
 Bigm
 Bigr
 biggl
 biggm
 biggr
 Biggl
 Biggm
 Biggr)

(provide-macros
 alpha
 beta
 gamma
 delta
 epsilon
 zeta
 eta
 theta
 iota
 kappa
 lambda
 mu
 nu
 xi
 pi
 rho
 sigma
 tau
 upsilon
 phi
 chi
 psi
 omega
 varepsilon
 vartheta
 varpi
 varrho
 varsigma
 varphi
 Gamma
 Delta
 Theta
 Lambda
 Xi
 Pi
 Sigma
 Upsilon
 Phi
 Psi
 Omega
 aleph
 imath
 jmath
 ell
 wp
 Re
 Im
 partial
 infty
 prime
 emptyset
 nabla
 top
 bot
 triangle
 forall
 exists
 neg
 lnot
 flat
 natural
 sharp
 clubsuit
 diamondsuit
 heartsuit
 spadesuit
 hbar
 surd
 angle
 bigvee
 bigwedge
 biguplus
 bigcap
 bigcup
 intop
 int
 prod
 sum
 bigotimes
 bigoplus
 bigodot
 oint
 bigsqcup
 smallint
 triangleleft
 triangleright
 bigtriangleup
 bigtriangledown
 wedge
 vee
 land
 lor
 cap
 cup
 ddagger
 dagger
 sqcap
 sqcup
 uplus
 amalg
 diamond
 bullet
 wr
 div
 odot
 oslash
 otimes
 ominus
 oplus
 mp
 pm
 circ
 bigcirc
 setminus
 cdot
 ast
 times
 star
 propto
 sqsubseteq
 sqsupseteq
 parallel
 mid
 dashv
 vdash
 nearrow
 searrow
 nwarrow
 swarrow
 Leftrightarrow
 Leftarrow
 Rightarrow
 neq
 ne
 leq
 geq
 le
 ge
 succ
 prec
 approx
 succeq
 preceq
 supset
 subset
 supseteq
 subseteq
 in
 ni
 owns
 gg
 ll
 not
 leftrightarrow
 leftarrow
 rightarrow
 gets
 to
 mapsto
 sim
 simeq
 perp
 equiv
 asymp
 smile
 frown
 leftharpoonup
 leftharpoondown
 rightharpoonup
 rightharpoondown
 cong
 notin
 rightleftharpoons
 doteq
 hookrightarrow
 hookleftarrow
 bowtie
 models
 Longrightarrow
 longrightarrow
 longleftarrow
 Longleftarrow
 longmapsto
 longleftrightarrow
 Longleftrightarrow
 iff
 ldotp
 cdotp
 colon
 ldots
 cdots
 vdots
 ddots)

(provide (rename-out [tex:stackrel stackrel]))
(define (tex:stackrel [top : MathTeX+Like] [body : MathTeX+Like])
  (macro "stackrel" (argument top) (argument body)))

(provide-macros-1
 smash)

(provide-macros
 \,
 thinspace
 >
 \:
 medspace
 \;
 thickspace
 enspace
 quad
 qquad
 | |
 nobreakspace
 space
 !
 negthinspace
 negmedspace
 negthickspace
 mathstrut)

(provide-macros-1
 phantom
 hphantom
 vphantom)

(provide (rename-out [tex:frac frac]))
(define (tex:frac [num : MathTeX+Like] [den : MathTeX+Like])
  (macro "frac" (argument num) (argument den)))

(define ((make-binom-cmd [a : String])
         [x : MathTeX+Like] [y : MathTeX+Like])
  (group x (macro a) y))

(define-syntax (provide-binom-macro stx)
  (make-provide-syntax stx #'make-binom-cmd))

(define-syntax-rule (provide-binom-macros id ...)
  (begin (provide-binom-macro id) ...))

(provide-binom-macros
 choose
 brack
 brace)

(provide-macros
 log
 lg
 ln
 lim
 limsup
 liminf
 sin
 arcsin
 sinh
 cos
 arccos
 cosh
 tan
 arctan
 tanh
 cot
 coth
 sec
 csc
 max
 min
 sup
 inf
 arg
 ker
 dim
 hom
 det
 exp
 Pr
 gcd
 deg
 bmod
 pmod)

(provide (rename-out [tex:sqrt sqrt]))
(define (tex:sqrt #:base [base : (Option MathTeX+Like) #f]
                  [body : MathTeX+Like])
  (define a (argument body))
  (if base
      (macro "sqrt" (optional-argument base) a)
      (macro "sqrt" a)))

(provide-macros-1
 mathbin
 mathclose
 mathinner
 mathop
 mathopen
 mathord
 mathpunct
 mathrel
 mathrm
 mathnormal
 textrm
 textnormal
 mathsf
 textsf
 mathbf
 textbf
 textmd
 mathtt
 texttt
 mathit
 textit
 textup
 mathcal)

(provide-macros
 displaystyle
 textstyle
 scriptstyle
 scriptscriptstyle
 limits
 nolimits)
