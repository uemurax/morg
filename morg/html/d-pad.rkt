#lang typed/racket

(require "../data/node.rkt"
         "../data/section.rkt"
         "../data/document.rkt"
         "../markup/inline.rkt"
         "../markup/xexpr.rkt"
         "../markup/splice.rkt"
         "inline.rkt"
         "site-state.rkt"
         "id.rkt"
         "class/d-pad.rkt")

(struct d-pad-config
  ([previous : PureInlineLike]
   [up : PureInlineLike]
   [next : PureInlineLike])
  #:transparent
  #:type-name D-padConfig)

(define default-d-pad-config
  (d-pad-config
   "←" "↑" "→"))

(define (make-d-pad #:config [cfg : D-padConfig default-d-pad-config]
                    [st : SiteState]
                    [n : Node]) : XExprs
  (define f (compose pure-inline->xexprs pure-inline%))
  (define previous-id
    (and (section-node? n)
         (let ([l (section-node-siblings-left n)])
           (and (not (null? l))
                (section-id (car l))))))
  (define next-id
    (and (section-node? n)
         (let ([r (section-node-siblings-right n)])
           (and (not (null? r))
                (section-id (car r))))))
  (define parent (node-parent n))
  (define up-id
    (cond
     [(section-node? parent) (node-id parent)]
     [else (document-id (site-state-root st))]))
  (tagged% 'ol
           `((class ,d-pad-class-name))
           (when% previous-id
             (tagged% 'li
                      `((class ,d-pad-previous-class-name))
                      (tagged% 'a
                               `((href ,(id->url previous-id)))
                               (f (d-pad-config-previous cfg)))))
           (tagged% 'li
                    `((class ,d-pad-up-class-name))
                    (tagged% 'a
                             `((href ,(id->url up-id)))
                             (f (d-pad-config-up cfg))))
           (when% next-id
             (tagged% 'li
                      `((class ,d-pad-next-class-name))
                      (tagged% 'a
                               `((href ,(id->url next-id)))
                               (f (d-pad-config-next cfg)))))))
