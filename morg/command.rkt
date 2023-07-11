#lang typed/racket

(require (prefix-in latex: "latex/config.rkt")
         (prefix-in html: "html/config.rkt")
         "data/document.rkt"
         "markup/syntax.rkt"
         "latex.rkt"
         "html.rkt")

(define (get-html-config [mod : (Option Module-Path)]) : html:Config
  (if mod
      (html:dynamic-require-config mod)
      html:default-config))

(define (get-latex-config [mod : (Option Module-Path)]) : latex:Config
  (if mod
      (latex:dynamic-require-config mod)
      latex:default-config))

(struct cmd-args
  ([html? : Boolean]
   [pdf? : Boolean]
   [html-config-file : (Option Module-Path)]
   [pdf-config-file : (Option Module-Path)]
   [index-file : Module-Path]
   [dst-dir : String]))

(module+ main
  (define args
    (let ([html? : (Parameterof Boolean) (make-parameter #f)]
          [pdf? : (Parameterof Boolean) (make-parameter #f)]
          [html-config-file : (Parameterof (Option Module-Path))
           (make-parameter #f)]
          [pdf-config-file : (Parameterof (Option Module-Path))
           (make-parameter #f)])
      (command-line
       #:once-each
       ["--html" "Build HTML version."
                 (html? #t)]
       ["--html-config" file
                        "Config file for HTML output. Implies --html."
                        (html? #t)
                        (html-config-file (string->path (assert file string?)))]
       ["--pdf" "Build PDF version."
                (pdf? #t)]
       ["--pdf-config" file
                       "Config file for PDF output. Implies --pdf."
                       (pdf? #t)
                       (pdf-config-file (string->path (assert file string?)))]
       #:usage-help
       "Build a MOrg document."
       "<index-file> is a module that exports a document."
       "<dst-dir> is a destination directory."
       #:args (index-file dst-dir)
       (cmd-args
        (html?)
        (pdf?)
        (html-config-file)
        (pdf-config-file)
        (string->path (assert index-file string?))
        (assert dst-dir string?)))))
  (define index-file (cmd-args-index-file args))
  (define dst-dir (cmd-args-dst-dir args))
  (define html? (cmd-args-html? args))
  (define pdf? (cmd-args-pdf? args))
  (define html-config-file (cmd-args-html-config-file args))
  (define pdf-config-file (cmd-args-pdf-config-file args))
  (displayln (format "Build HTML?: ~a" html?))
  (when html?
    (displayln (format "HTML config file: ~a" html-config-file)))
  (displayln (format "Build PDF?: ~a" pdf?))
  (when pdf?
    (displayln (format "PDF config file: ~a" pdf-config-file)))
  (displayln (format "Index file: ~a" index-file))
  (displayln (format "Destination directory: ~a" dst-dir))
  (define doc (assert (dynamic-include-part index-file) document?))
  (when html?
    (->html/publish #:config (get-html-config html-config-file)
                    doc dst-dir))
  (when pdf?
    (->latex/publish #:config (get-latex-config pdf-config-file)
                     doc dst-dir)))
