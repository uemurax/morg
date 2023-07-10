#lang typed/racket

(module+ main
  (define html? : (Parameterof Boolean)
    (make-parameter #f))
  (define pdf? : (Parameterof Boolean)
    (make-parameter #f))
  (define html-config-file : (Parameterof (Option String))
    (make-parameter #f))
  (define pdf-config-file : (Parameterof (Option String))
    (make-parameter #f))
  (define args
    (command-line
     #:once-each
     ["--html" "Build HTML version."
               (html? #t)]
     ["--html-config" file
                      "Config file for HTML output. Implies --html."
                      (html? #t)
                      (html-config-file (cast file String))]
     ["--pdf" "Build PDF version."
              (pdf? #t)]
     ["--pdf-config" file
                     "Config file for PDF output. Implies --pdf."
                     (pdf? #t)
                     (pdf-config-file (cast file String))]
     #:args (index-file dst-dir)
     (list index-file dst-dir)))
  (define index-file
    (cast (list-ref args 0) String))
  (define dst-dir
    (cast (list-ref args 1) String))
  (displayln (format "Build HTML?: ~a" (html?)))
  (when (html?)
    (displayln (format "HTML config file: ~a" (html-config-file))))
  (displayln (format "Build PDF?: ~a" (pdf?)))
  (when (pdf?)
    (displayln (format "PDF config file: ~a" (pdf-config-file))))
  (displayln (format "Index file: ~a" index-file))
  (displayln (format "Destination directory: ~a" dst-dir)))
