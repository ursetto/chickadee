;; todo
;;   rewrite /cdoc/path/to/node to ?path=path to node, somehow
;;   ----
;;   bench

(use spiffy spiffy-request-vars html-tags html-utils chicken-doc)
(use matchable)
(use (only uri-generic uri-encode-string))
(use uri-common)
(use intarweb)
;(load "chicken-doc-html.scm")
(require-library chicken-doc-html)
(import chicken-doc-html) ; temp -- for awful reload

;; (page-exception-message
;;  (lambda (exn)
;;    (<pre> convert-to-entities?: #t
;;           (with-output-to-string
;;             (lambda ()
;;               (print-call-chain)
;;               (print-error-message exn))))))

(define (input-form)
  (<form> class: "lookup"
          action: (cdoc-page-path)
          method: 'get
          (<input> class: "text" type: "text" name: "q")  ; query should redirect.
          (<input> class: "button" type: "submit" name: "query-name" value: "Lookup")
          (<input> class: "button" type: "submit" name: "query-regex" value: "Regex")))

;; Really needs to redirect to (or at least call) format-path
(define (format-id x)
  (match (match-nodes x)
         ((n1)
          (redirect-to (path->href (node-path n1))))
         (()
          (node-page #f
                     ""
                     (<p> "No node found matching identifier " (<tt> x))))
         (nodes
          (match-page nodes x))))

(define (format-re x)
  (match-page (match-nodes (irregex x)) x))

(define (match-page nodes match-text)
  (node-page (string-append "query " match-text " ("
                            (number->string (length nodes))
                            " matches)")
             ""
             (apply <table>   ; yuck
                    class: "match-results"
                    (<tr> (<th> "path") (<th> "signature"))
                    (map (lambda (n)
                           (<tr> (<td> class: "match-path" (title-path n))
                                 (<td> class: "match-sig"
                                       (<a> href: (path->href (node-path n))
                                            (<tt> convert-to-entities?: #t
                                                  (node-signature n))))))
                         nodes))))

;; contents doesn't need full path like match page
;; (define (contents-page n)
;;   (match-page (node-children n)
;;               (title-path n)         ; not correct
;;               ))

(define (contents-list n)
  (let ((p (map ->string (node-path n))))
    (tree->string
     `("<ul class=\"contents-list\">"
       ,(map
         (lambda (id)
           `("<li>"
             "<a href=\"" ,(path->href (append p (list id)))
             "\">" ,(quote-html id)
             "</a>"
             "</li>"))
         (map ->string (node-child-ids n)))
       "</ul>"
       ))))

(define (format-path p)
  (let ((n (handle-exceptions e #f (lookup-node (string-split p)))))
    (with-request-vars
     (contents)           ; bad
     (if n
         (if contents
             (contents-page n)
             (node-page (string-append (title-path n)
                                       ""
                                       ;; " | "
                                       ;; ;; don't know how to do this right now
                                       ;; "<a href=\"?path=" (uri-encode-string p)
                                       ;; "&contents=1\">contents</a>"
                                       )
                        (contents-list n)
                        (chicken-doc-sxml->html (node-sxml n))))
         (node-page p "" (<p> "No node found at path " (<i> p)))))))

(define (path->href p)
  (string-append
   (chickadee-page-path)
   "/"
   (string-intersperse (map (lambda (x)
                              (uri-encode-string (->string x)))
                            p) "/")))
(define (title-path n)
  (let loop ((p (node-path n))
             (f '())
             (r '()))
    (if (null? p)
        (tree->string (reverse r))
        (let* ((id (->string (car p)))
               (f (append f (list id)))
               (n (lookup-node f)))
          (loop (cdr p) f (cons
                           (list
                            "<a href=\"" (path->href f)
                            "\">" (quote-html id)
                            "</a>"
                            (if (null? (cdr p)) '() " &raquo; "))
                           r))))))

(define (query p)
  (let ((q (string-split p)))
    (cond ((null? q)
           (redirect-to (cdoc-page-path)))
          ((null? (cdr q))
           (format-id p))
          (else
           (redirect-to (path->href q))
           ;; (format-path p)
           ))))                 ;  API defect

(define (root-page)
  (++ (<h3> "Search")
      (input-form)
      (<p> "Enter a documentation node name or path in the search box above."
           (<ul> (<li> "A node name is a single word, usually an identifier or egg name.  Examples: "
                       (<tt> (<u> "posix")) ", " (<tt> (<u> "open/rdonly"))
                       ", " (<tt> (<u> "+")) ".")
                 (<li> "A node path is multiple words, separated by spaces, such as "
                       (<tt> (<u> "posix open/rdonly")) ".")))

      (<h3> "Quick links")
      (<ul> (<li> (<a> href: (path->href '(chicken)) "Chicken manual"))
            (<li> (<a> href: (path->href '(chicken language)) "Supported language"))
            (<li> (<a> href: (path->href '(foreign)) "FFI"))
                  ))
)

(define (uri-path->string p)   ; (/ "foo" "bar") -> "/foo/bar"
  (uri->string (update-uri (uri-reference "")
                           path: p)))

(define cdoc-page-path (make-parameter #f))
(define cdoc-uri-path
  (make-parameter #f
                  (lambda (x) (cdoc-page-path
                          (and x (uri-path->string x))))))

(define chickadee-page-path (make-parameter #f))
(define chickadee-uri-path
  (make-parameter #f
                  (lambda (x) (chickadee-page-path   ;auto update (mostly for debugging)
                          (and x (uri-path->string x))))))

(handle-not-found
 (let ((old-handler (handle-not-found)))  ; don't eval more than once!
   (lambda (path) ; useless
     (let ((p (uri-path (request-uri (current-request)))))
       (parameterize ((http-request-variables (request-vars))) ; for $
         (match p
                (('/ "cdoc" . p)
                 (cdoc-handler p))
                (('/ "chickadee" . p)
                 (chickadee-handler p))
                (else (old-handler path))))))))

;; uri rewriter
(define (chickadee-handler p)
  (let ((r (current-request)))
    (restart-request (make-request
                      uri: (uri-reference
                            ;; FIXME
                            (if (null? p)
                                "http://localhost:8080/cdoc"
                                (string-append "http://localhost:8080/cdoc?path="
                                               (uri-encode-string
                                                (string-intersperse p
                                                                    " ")))))
                      headers: (request-headers r)))))

(define (cdoc-handler p)
  p ;ignore
  (with-request-vars
   $ (id path q)
     (cond (path => format-path)
           (id   => format-id)
           (q    => (lambda (p)
                      (with-request-vars
                       $ (query-regex query-name)
                       (if query-regex
                           (format-re p)
                           (query p)))))
           (else
            (node-page #f (contents-list (lookup-node '()))
                       (root-page))))))

(define ++ string-append)  ; legacy from awful
(define (link href desc)
  (<a> href: href desc))
(define ($ var #!optional default converter)  ; from awful
    ((http-request-variables) var default (or converter identity)))
(define http-request-variables (make-parameter #f))

(define (node-page title contents body)
  (send-response
   body:
   (html-page
    (++ (<h1> (link (chickadee-page-path) "chickadee")
              (if title
                  (string-append " &raquo; " title)
                  (string-append " | chicken-doc server")))
        (<div> id: "contents"
               contents)
        (<div> id: "body"
               (<div> id: "main"
                      body)))
    css: "/awful-cdoc.css")))

;; missing full node path should generate 404
;; "q" search should operate like chicken-doc cmd line

(define (redirect-to path #!key (code 302) (headers '()))
  (send-response code: code
                 headers: `((location ,(uri-relative-to
                                        (uri-reference path)
                                        (server-root-uri)))
                            . ,headers)))


;;; start server

(root-path ".")  ; dangerous
(debug-log (current-error-port))
(server-port 8080)
(tcp-buffer-size 1024)

(cdoc-uri-path '(/ "cdoc"))
(chickadee-uri-path '(/ "chickadee"))

(verify-repository)
(start-server)

#|
jim@amaranth ~$ time echo "GET /cdoc?q=fmt+abc HTTP/1.0" | nc localhost 8080
Location: http://localhost:8080/cdoc?path=fmt%20abc
real    0m0.024s

jim@amaranth ~$ time echo "GET /cdoc?q=fmt HTTP/1.0" | nc localhost 8080
Location: http://localhost:8080/cdoc?path=fmt
real    0m0.145s
|#
