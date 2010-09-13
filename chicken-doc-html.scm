(module chicken-doc-html
(chicken-doc-sxml->html
 tree->string quote-html
 quote-identifier unquote-identifier definition->identifier signature->identifier)

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
(use (only srfi-13 string-downcase))

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

;; Like sxpath // *text*.  Beware, if your tags have arguments that
;; shouldn't be considered text, they will still be extracted.
(define (text-content doc)
  (tree->string
   (sxml-walk doc `((*default* . ,(lambda (t b s) (sxml-walk b s)))
                    (*text* . ,(lambda (t b s) b))))))

;;; URI fragment (id=) handling for sections and definitions
;; Permitted characters in ID attributes in HTML < 5 are only A-Z a-z 0-9 : - _
;; even though URI fragments are much more liberal.  For compatibility, we
;; "period-encode" all other chars.
(define +rx:%idfragment-escape+ (irregex "[^-_:A-Za-z0-9]"))
(define +rx:%idfragment-unescape+ (irregex "\\.([0-9a-fA-F][0-9a-fA-F])"))
;; Encode raw identifier text string so it is usable as an HTML 4 ID attribute
;; (and consequently, as a URI fragment).
(define (quote-identifier x)  ; Not a good name; should prob. be encode-identifier
  (irregex-replace/all
   +rx:%idfragment-escape+ x
   (lambda (m) (sprintf ".~x"
                   (char->integer
                    (string-ref (irregex-match-substring m 0) 0))))))
;; Decode period-encoded URI fragment (or ID attribute value).
;; Note that spaces were period-encoded, not converted to underscore,
;; so the transformation is reversible.
(define (unquote-identifier x)
  (irregex-replace/all +rx:%idfragment-unescape+ x
                       (lambda (m) (string
                               (integer->char
                                (string->number (irregex-match-substring m 1)
                                                16))))))
;; WARNING: Currently being used to both generate new ids for headers and
;; to figure out the id for an internal-link target.  However the former may
;; distinuish duplicate IDs while the latter should ignore duplicates.
;; FIXME: Duplicate IDs will be generated for duplicate section or
;; definition names.  A unique suffix is needed.
(define (section->identifier x)
  (string-append "sec:"
                 (string-translate x #\space #\_)))
(define (definition->identifier x)
  (string-append "def:" x))
(define (section->href x)   ;; Convert section name to internal fragment href.
  (string-append "#" (quote-identifier
                      (section->identifier x))))

(use (only svnwiki-sxml svnwiki-signature->identifier))
(define signature->identifier svnwiki-signature->identifier)

;;; HTML renderer

(define +rx:wiki-man-page+ (irregex '(: (? "http://wiki.call-cc.org")
                                        (or "/man/4/"
                                            "/manual/")
                                        (submatch (+ any)))))
(define +rx:wiki-egg-page+ (irregex '(: (? "http://wiki.call-cc.org")
                                        (or "/eggref/4/"
                                            "/egg/")
                                        (submatch (+ any)))))
(define (chicken-doc-sxml->html doc
                                path->href ; for internal links; make parameter?
                                def->href ; link to definition node
                                )
  (tree->string
   (let ((walk sxml-walk)
         (drop-tag (lambda (t b s) '()))
         (drop-tag-noisily (lambda (t b s) (warning "dropped" (cons t b)) '()))
         (quote-text `(*text* . ,(lambda (t b s) (quote-html b)))))
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
                                      (let ((link (lambda (href desc)  ;; Caller must quote DESC.
                                                    (let ((href
                                                           ;; svnwiki-sxml does not return int-link for
                                                           ;; call-cc.org links, so we must check that here.
                                                           (cond
                                                            ;; Wiki man page, link to corresponding man page
                                                            ((string-match +rx:wiki-man-page+ href)
                                                             => (lambda (m)
                                                                  (cond ((man-filename->path (cadr m))
                                                                         => path->href)
                                                                        (else href))))
                                                            ;; Wiki egg page, link to node
                                                            ((string-match +rx:wiki-egg-page+ href)
                                                             => (lambda (m)
                                                                  (path->href (list (cadr m)))))
                                                            (else href))))
                                                      `("<a href=\"" ,(quote-html href) "\">" ,desc "</a>")))))
                                          (match b
                                                 ((href desc)
                                                  (link href (walk desc inline-ss)))
                                                 ((href)
                                                  (link href (quote-html href)))))))
                           (int-link
                            . ,(lambda (t b s)
                                 (let ((ilink
                                        (lambda (link desc)   ;; Caller must quote DESC.
                                          (let ((href
                                                 ;; Usage of man-filename->path is barely tolerable.
                                                 ;; Perhaps we should use the id cache.
                                                 (cond ((char=? (string-ref link 0)
                                                                #\#)
                                                        ;; Assume #fragments target section names in this doc.
                                                        (section->href (substring link 1)))
                                                       ;; Wiki man page, link to corresponding man page
                                                       ((string-match +rx:wiki-man-page+ link)
                                                        => (lambda (m)
                                                             (cond ((man-filename->path (cadr m))
                                                                    => path->href)
                                                                   (else link))))
                                                       ;; Wiki egg page, link to node
                                                       ((string-match +rx:wiki-egg-page+ link)
                                                        => (lambda (m)
                                                             (path->href (list (cadr m)))))
                                                       ;; Unknown absolute path, link to wiki
                                                       ((char=? (string-ref link 0)
                                                                #\/)
                                                        (string-append ; ???
                                                         "http://wiki.call-cc.org"
                                                         link))
                                                       ;; Relative path, try man page.  Wiki links to
                                                       ;; current directory (/man) but we can't.
                                                       ((man-filename->path link)
                                                        => path->href)
                                                       ;; Relative path, assume egg node.
                                                       (else
                                                        (path->href (list link)) ; !
                                                        ))))
                                            `("<a href=\"" ,(quote-html href) "\">" ,desc "</a>")))))
                                   (match b
                                          ((link desc) (ilink link (walk desc inline-ss)))
                                          ((link) (ilink link (quote-html link)))))))))
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
                                        ((type sig . alist)
                                         (let* ((defid (cond ((assq 'id alist) => cadr)
                                                             (else (signature->identifier sig type))))
                                                (defid (and defid (->string defid))))
                                           `("<dt class=\"defsig\""
                                             ,(if defid
                                                  `(" id=\""
                                                    ,(quote-identifier
                                                      (definition->identifier defid))
                                                    #\")
                                                  '())
                                             #\>
                                             ;; Link to underlying node.
                                             ,(if defid
                                                  `("<a href=" #\"
                                                    ,(def->href defid)
                                                    #\" #\>)
                                                  '())
                                             "<span class=\"sig\"><tt>"
                                             ,(quote-html sig) "</tt></span>"
                                             ,(if defid "</a>" '())
                                             " "
                                             "<span class=\"type\">"
                                             ,(quote-html (->string type))
                                             "</span>"
                                             "</dt>\n")))))
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
                                            "h" (number->string level)))
                                        (id (cond ((section->identifier
                                                    (text-content title))
                                                   => quote-identifier)
                                                  (else #f))))
                                    (list "<" H
                                          (if id `(" id=\"" ,id "\"") '())
                                          ">"
                                          "<a href=\"#" id "\">"
                                          (walk title inline-ss)
                                          "</a>"
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
                                    (let ((lang (and lang (string->symbol
                                                           (string-downcase
                                                            (->string lang))))))
                                      (if (and lang     ;; lang #f not currently possible; reserved for future
                                               (coloring-type-exists? lang))
                                          (list "<pre class=\"highlight\">"
                                                (html-colorize lang
                                                               ;; html-colorize quotes HTML; don't walk
                                                               (tree->string body))
                                                "</pre>")
                                          (list (if lang
                                                    (list "<!-- Unknown coloring type "
                                                          (quote-html (->string lang)) " -->\n")
                                                    '())
                                                "<pre class=\"highlight\">"
                                                (walk body s))))))))

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



