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
(use chicken-doc-html)

;;; Pages

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

(define (path->href p)             ; FIXME: use uri-relative-to, etc
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
    css: (chickadee-css-path))))

(define cdoc-page-path (make-parameter #f))
(define cdoc-uri-path
  (make-parameter #f (lambda (x)
                       (cdoc-page-path
                        (and x (uri-path->string x)))
                       x)))

(define chickadee-page-path (make-parameter #f))
(define chickadee-uri-path
  (make-parameter #f
                  (lambda (x)
                    (chickadee-page-path   ;auto update (mostly for debugging)
                     (and x (uri-path->string x)))
                    x)))

(define chickadee-css-path
  (make-parameter "chickadee.css"))

;;; Helper functions

;; Inefficient way to functionally update an alist.
(define (alist-update k v x . test)
  (apply alist-update! k v (alist-copy x) test))
(define (update-param param val uri-query)
  (alist-update param val uri-query))
(define (update-request-uri r u)
  (make-request uri: u
                ;; No easy way to update request URI.
                port: (request-port r)
                method: (request-method r)
                major: (request-major r)
                minor: (request-minor r)
                headers: (request-headers r)))
(define (match-path pattern path)  ; just a prefix match on list; returns remainder
  (let loop ((pattern pattern)
             (path path))
    (cond ((null? path)
           (and (null? pattern)
                path))
          ((null? pattern)
           path)
          ((equal? (car pattern)
                   (car path))                              ; allow re match?
           (loop (cdr pattern) (cdr path)))
          (else #f))))
;; calls rewriter with the current uri; restarts request with the returned uri
(define (rewrite-uri rewriter)
  (let* ((r (current-request))
         (u (request-uri r)))
    (restart-request
     (update-request-uri r (rewriter u)))))

(define ++ string-append)  ; legacy from awful
(define (link href desc)
  (<a> href: href desc))
(define ($ var #!optional default converter)  ; from awful
    ((http-request-variables) var default (or converter identity)))
(define http-request-variables (make-parameter #f))

;; note: missing full node path should maybe generate 404
(define (redirect-to path #!key (code 302) (headers '()))
  (send-response code: code
                 headers: `((location ,(uri-relative-to
                                        (uri-reference path)
                                        (server-root-uri)))
                            . ,headers)))
(define (uri-path->string p)   ; (/ "foo" "bar") -> "/foo/bar"
  (uri->string (update-uri (uri-reference "")
                           path: p)))

;;;

(define (rewrite-chickadee-uri u p)
  (let ((q (uri-query u)))
    (update-uri u
                path: (cdoc-uri-path)
                query: (if (null? p)
                           q
                           (update-param 'path
                                         (string-intersperse p " ") q)))))



(define (chickadee-handler p)
  (rewrite-uri (lambda (u) (rewrite-chickadee-uri u p)))) ;?

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

;;; set up handlers

(handle-not-found
 (let ((old-handler (handle-not-found)))  ; don't eval more than once!
   (lambda (path) ; useless
     (let ((p (uri-path (request-uri (current-request)))))
       (parameterize ((http-request-variables (request-vars))) ; for $
         (cond ((match-path (cdoc-uri-path) p)
                => cdoc-handler)
               ((match-path (chickadee-uri-path) p)
                => chickadee-handler)
               (else
                (old-handler path))))))))

;;; start server

(root-path ".")  ; dangerous
(debug-log (current-error-port))
(server-port 8080)
(tcp-buffer-size 1024)

(cdoc-uri-path '(/ "cdoc"))
(chickadee-uri-path '(/ "chickadee"))
(chickadee-css-path "/chickadee.css")

(verify-repository)
(start-server)
