(module chicken-doc-html
(chicken-doc-sxml->html
 tree->string quote-html)

(import scheme chicken)
(use (only sxml-transforms string->goodHTML SRV:send-reply))                 ; temp
(use (only uri-generic uri-encode-string)) ; grr
(use matchable)
(use (only data-structures conc ->string))
(use (only ports with-output-to-string))
(use regex) (import irregex)

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

(define (quote-html s)
  (string->goodHTML s))

(define (chicken-doc-sxml->html doc)
  (tree->string
   (let ((walk sxml-walk)
         (drop-tag (lambda (t b s) '()))
         (quote-text `(*text* . ,(lambda (t b s) (string->goodHTML b))))
         (link (lambda (href desc)
                 `("<a href=\"" ,href "\">"
                   ,(quote-html desc)
                   "</a>"))))
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
              (inline-ss `(
                           ,quote-text
                           (b . ,(inline "b"))
                           (i . ,(inline "i"))
                           (tt . ,(inline "tt"))
                           (img . ,drop-tag)
                           (link . ,(lambda (t b s)
                                      (match b
                                             ((href desc)
                                              (link href desc))
                                             ((href)
                                              (link href href)))))
                           (int-link . ,(lambda (t b s)
                                          (let ((ilink (lambda (href desc)
                                                         ;; Incomplete.  We need to
                                                         ;; rewrite internal and
                                                         ;; external wiki links to
                                                         ;; local nodes, including
                                                         ;; canonical node names.
                                                         `("<a href=\"/cdoc?path="
                                                           ,(uri-encode-string href)
                                                           "\">" ,(quote-html desc)
                                                           "</a>"))))
                                            (match b
                                                   ((href desc) (ilink href desc))
                                                   ((href) (ilink href href))))))))
              )
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
                                           ,(string->goodHTML sig) "</tt></span>"
                                           " "
                                           "<span class=\"type\">"
                                           ,(string->goodHTML (->string type))
                                           "</span>"
                                           "</dt>\n"))))
                               sigs)
                             "<dd class=\"defsig\">"
                             ,(walk body def-ss)
                             "</dd>\n")))
                  "</dl>\n")))
          (pre . ,(block "pre"))   ; may need to quote contents
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
                                           "</dd>")))))
                     "</dl>\n")))

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

          (table . ,(lambda (t b table-ss)
                      `("<table>\n"
                        ,(walk b `((tr . ,(lambda (t b s)
                                            `("<tr>"
                                              ,(walk b
                                                     `((th . ,(lambda (t b s)
                                                                `("<th>"
                                                                  ,(walk b table-ss)
                                                                  "</th>")))
                                                       (td . ,(lambda (t b s)
                                                                `("<td>"
                                                                  ,(walk b table-ss)
                                                                  "</td>")))))
                                              "</tr>\n")))
                                   (@ . ,drop-tag)))
                        "</table>\n")))
          
          (script . ,(lambda (t b s)
                       (match b ((lang . body)
                                 (list "<pre>" (walk body s) "</pre>")))))

          ;; convert example contents to `(pre ...) and re-walk it
          (examples
           . ,(lambda (t b ex-ss)
                (walk b `((example
                           . ,(lambda (t b s)
                                (walk `(pre
                                        ,(walk b
                                               `((init . ,(lambda (t b s)
                                                            (list b "\n")))
                                                 (expr . ,(lambda (t b s) b))
                                                 (result . ,(lambda (t b s)
                                                              `("\n; Result: " ,b)))
                                                 )))
                                      ex-ss)))))))

          (blockquote . ,(block "blockquote"))

          (hr . ,(lambda (t b s)
                   "<hr />"))
          
          ,@inline-ss
          ))))))


)



