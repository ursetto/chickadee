(module chicken-doc-html
(chicken-doc-sxml->html
 tree->string quote-html)

(import scheme chicken)
(use (only sxml-transforms string->goodHTML SRV:send-reply))                 ; temp
(use (only uri-generic uri-encode-string)) ; grr
(use matchable)
(use (only data-structures conc ->string string-intersperse string-translate))
(use (only ports with-output-to-string))
(use (only chicken-doc-admin man-filename->path))
(use colorize) ;yeah!
(use regex) (import irregex)
(use (only extras sprintf))

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

;;; URI fragment (id=) handling for sections and definitions
(define +rx:%fragment-escape+ (irregex "[^-_:A-Za-z0-9]"))
(define +rx:%fragment-unescape+ (irregex "\\.([0-9a-fA-F][0-9a-fA-F])"))
(define (identifier->fragment x)
  (irregex-replace/all
   +rx:%fragment-escape+ x
   (lambda (m) (sprintf ".~x"
                   (char->integer
                    (string-ref (irregex-match-substring m 0) 0))))))
(define (fragment->identifier x)
  (irregex-replace/all +rx:%fragment-unescape+ x
                       (lambda (m) (string
                               (integer->char
                                (string->number (irregex-match-substring m 1)
                                                16))))))
(define (section->identifier x)
  (string-append "sec:"
                 (string-translate x #\space #\_)))

(define (chicken-doc-sxml->html doc
                                path->href ; for internal links; make parameter?
                                )
  (tree->string
   (let ((walk sxml-walk)
         (drop-tag (lambda (t b s) '()))
         (drop-tag-noisily (lambda (t b s) (warning "dropped" (cons t b)) '()))
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
                           (*default* . ,drop-tag-noisily) ;; 500 error is annoying
                           (b . ,(inline "b"))
                           (i . ,(inline "i"))
                           (tt . ,(inline "tt"))
                           (sup . ,(inline "sup"))
                           (sub . ,(inline "sub"))
                           (small . ,(inline "small")) ;; questionable
                           (big . ,(inline "big"))     ;; questionable
                           (img . ,drop-tag)
                           (link . ,(lambda (t b s)
                                      (match b
                                             ((href desc)
                                              (link href desc))
                                             ((href)
                                              (link href href)))))
                           (int-link
                            . ,(lambda (t b s)
                                 (let ((ilink
                                        (lambda (href desc)
                                          (let ((href
                                                 ;; barely tolerable.  perhaps we
                                                 ;; should use the id cache
                                                 (cond ((man-filename->path href)
                                                        => path->href)
                                                       ((char=? (string-ref href 0)
                                                                #\#)
                                                        href)
                                                       ((char=? (string-ref href 0)
                                                                #\/)
                                                        (string-append ; ???
                                                         "http://chicken.wiki.br/"
                                                         href))
                                                       (else
                                                        (path->href (list href)) ; !
                                                        ))))
                                            `("<a href=\"" ,href "\">"
                                              ,(quote-html desc)
                                              "</a>")))))
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
          (pre . ,(block "pre"))        ; may need to quote contents
          (ul . ,(lambda (t b ul-ss)
                   `("<ul>"
                     ,(walk b `((li
                                 . ,(lambda (t b s)
                                      `("<li>"
                                        ,(walk b ul-ss)
                                        "</li>\n")))))
                     "</ul>\n")))
          (ol . ,(lambda (t b ol-ss)
                   `("<ol>"
                     ,(walk b `((li
                                 . ,(lambda (t b s)
                                      `("<li>"
                                        ,(walk b ol-ss)
                                        "</li>\n")))))
                     "</ol>\n")))
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
                                  (let ((H (list
                                            "h" (number->string level)
                                            ;; FIXME title markup must be stripped!
                                            ))
                                        (id (cond ((section->identifier title)
                                                   => (lambda (x)
                                                        `(" id=" #\"
                                                          ,(identifier->fragment x)
                                                          #\")))
                                                  (else '()))))
                                    (list "<" H id ">"
                                          (walk title inline-ss)
                                          "</" H ">"
                                          (walk body s)))))))

          (table . ,(lambda (t b table-ss)
                      `("<table>\n"
                        ,(walk b `((tr . ,(lambda (t b s)
                                            `("<tr>"
                                              ,(walk b
                                                     (let ((table-ss `((@ . ,drop-tag)
                                                                       . ,table-ss)))
                                                       `((th . ,(lambda (t b s)
                                                                  `("<th>"
                                                                    ,(walk b table-ss)
                                                                    "</th>")))
                                                         (td . ,(lambda (t b s)
                                                                  `("<td>"
                                                                    ,(walk b table-ss)
                                                                    "</td>")))
                                                         (@ . ,drop-tag))))
                                              "</tr>\n")))
                                   (@ . ,drop-tag)))
                        "</table>\n")))
          
          (highlight . ,(lambda (t b s)
                       (match b ((lang . body)
                                 (list "<pre class=\"highlight\">"
                                       (html-colorize lang
                                                      ;; html-colorize quotes HTML; don't walk
                                                      (tree->string body))
                                       "</pre>")))))
          ;; script -- old name for highlight
          (script . ,(lambda (t b s)
                       (match b ((lang . body)
                                 (list "<pre>" (walk body s) "</pre>")))))

          ;; convert example contents to `(pre ...) and re-walk it
          
          ;; FIXME: The html-parser will erroneously parse html tags
          ;; inside <expr> tags.  Right now we drop them, but we
          ;; should either not parse them in the first place or
          ;; convert them back here (less nice).  Furthermore the parser
          ;; may unrecoverably screw up the structure of examples, for
          ;; example if it contains an <h1> tag; therefore we drop unknown
          ;; tags to prevent a complete rendering error.

          (examples
           . ,(lambda (t b ex-ss)
                (walk b `((*default* . ,drop-tag-noisily)
                          (example
                           . ,(lambda (t b s)
                                (walk `(pre
                                        ,(walk b
                                               `((init . ,(lambda (t b s)
                                                            (list b "\n")))
                                                 (expr . ,(lambda (t b s)
                                                            (walk b `((*default*
                                                                       . ,drop-tag-noisily)))))
                                                 (result . ,(lambda (t b s)
                                                              `("\n; Result: " ,b)))
                                                 (*default* . ,drop-tag-noisily))))
                                      ex-ss)))))))

          (blockquote . ,(block "blockquote"))

          (hr . ,(lambda (t b s)
                   "<hr />"))

          ,@inline-ss
          ))))))


)



