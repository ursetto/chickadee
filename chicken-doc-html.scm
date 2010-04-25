(module chicken-doc-html
*

(import scheme chicken)
(use sxml-transforms)   ; temp
(use matchable)
(use (only data-structures conc))
(use (only ports with-output-to-string))

(define (sxml-walk doc ss)
  (let ((default-handler (cond ((assq '*default* ss) => cdr)
                               (else
                                (lambda (t b s) (error 'sxml-walk
                                            "No default binding for" t)))))
        (text-handler (cond ((assq '*text* ss) => cdr)
                            (else #f))))
    (let loop ((doc doc))
      (cond ((null? doc) '())
            ((pair? doc)
             (let ((tag (car doc))
                   (body (cdr doc)))
               (if (symbol? tag)
                   (let ((handler-cell (assq tag ss)))
                     (if handler-cell
                         ((cdr handler-cell) tag body ss)
                         (default-handler tag body ss)))
                   (map loop doc))))
            (else
             (if text-handler
                 (text-handler '*text* doc ss)
                 doc))))))


(define (tree->string doc)
  (with-output-to-string (lambda () (SRV:send-reply doc))))

(define (chicken-doc-sxml->html doc)
  (tree->string
   (let ((walk sxml-walk)
         (drop-tag (lambda (t b s) '())))
     (letrec ((block (lambda (tag)
                       (let ((open (conc "<" tag ">"))
                             (close (conc "</" tag ">")))
                         (lambda (t b s) (list open
                                          (walk b s)
                                          close)))))
              (inline (lambda (tag)
                        (let ((open (conc "<" tag ">"))
                              (close (conc "</" tag ">")))
                          (lambda (t b s) (list open
                                           (walk b inline-ss)
                                           close)))))
              (inline-ss `((b . ,(inline "b"))
                           (i . ,(inline "i"))
                           (tt . ,(inline "tt"))
                           (link . ,(lambda (t b s)
                                      (match b
                                             ((href desc)
                                              `("<a href=\""
                                                ,href     ; FIXME: quote this
                                                "\">"
                                                ,desc
                                                "</a>"))
                                             ((href)
                                              `("<a href=\""
                                                ,href
                                                "\">"
                                                ,href
                                                "</a>")))))
                           (int-link . ,(lambda (t b s)
                                          (match b
                                                 ((href desc) desc)
                                                 ((href) href)))))))
       (walk
        doc
        `(
          (p . ,(inline "p"))

          (def
           . ,(lambda (t b def-ss)
                `("<dl class=\"defsig\">"
                  ,(match b
                          ((('sig . sigs) . body)
                           `(,(map
                               (lambda (s)
                                 (match s
                                        ((type sig)
                                         `("<dt class=\"defsig\">"
                                           "<span class=\"sig\"><tt>"
                                           ,sig "</tt></span>"
                                           " "
                                           "<span class=\"type\">" ,type "</span>"
                                           "</dt>\n"))))
                               sigs)
                             "<dd class=\"defsig\">"
                             ,(walk body def-ss)
                             "</dd>\n")))
                  "</dl>\n")))
          (pre . ,(block "pre"))
          (ul . ,(lambda (t b ul-ss)
                   `("<ul>"
                     ,(walk b `((li
                                 . ,(lambda (t b s)
                                      `("<li>"
                                        ,(walk b ul-ss)
                                        "</li>\n")))))
                     "</ul>\n")))
          (dl . ,(lambda (t b dl-ss)
                   `("<dl>"
                     ,(walk b `((dt . ,(lambda (t b s)
                                         `("<dt>"
                                           ,(walk b inline-ss) ;?
                                           "</dt>\n")))
                                (dd . ,(lambda (t b s)
                                         `("<dd>"
                                           ,(walk b dl-ss)
                                           "</dd>"))))))))

          (tags . ,drop-tag)
          (toc . ,drop-tag)
          (section . ,(lambda (t b s)
                        (match b ((level title . body)
                                  (let ((H (string-append "h"
                                                          (number->string level)
                                                          ">")))
                                    (list "<" H
                                          title
                                          "</" H
                                          (walk body s)))))))
          (script . ,(lambda (t b s)
                       (match b ((lang . body)
                                 (list "<pre>" (walk body s) "</pre>")))))

          (hr . ,(lambda (t b s)
                   "<hr />"))
          
          ,@inline-ss
          ))))))


)



