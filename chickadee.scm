;; chickadee chicken-doc server
;; Copyright (c) 2010 Jim Ursetto.  All Rights Reserved.
;; License: BSD.

(module chickadee
 (chickadee-start-server
  cdoc-uri
  chickadee-uri
  incremental-search-uri
  chickadee-css-files
  chickadee-early-js-files
  chickadee-js-files
  maximum-match-results
  maximum-match-signatures
  incremental-search
  incremental-search-delay
  cache-nodes-for
  cache-static-content-for
  last-modified
  ajax-log

  %chickadee:debug-incremental-search-latency
  )

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (import tcp data-structures srfi-1)
  (use spiffy-request-vars chicken-doc)
  (use spiffy)
  (use matchable)
  (use (only uri-generic uri-encode-string))
  (use uri-common)
  (use intarweb)
                                        ;(load "chicken-doc-html.scm")
  (use chicken-doc-html)
  (use (only chicken-doc-admin man-filename->path))
  (use regex) (import irregex)
  (use (only srfi-13 string-index string-concatenate))
  (use (only posix seconds->string seconds->utc-time utc-time->seconds))
  (use srfi-18)
  (use (only sxml-transforms
             pre-post-order* universal-conversion-rules* SRV:send-reply))
  (use (only ports with-output-to-port with-output-to-string))
  )
 (else
  (import (chicken base)
          (chicken condition)
          (chicken irregex)
          (chicken port)
          (chicken string)
          (chicken time)
          (chicken time posix)
          (chicken tcp)
          chicken-doc
          chicken-doc-html
          (only chicken-doc-admin man-filename->path)
          intarweb
          matchable
          regex             ;; FIXME remove -- replace string-substitute
          spiffy
          spiffy-request-vars
          srfi-1
          (only srfi-13 string-index string-concatenate)
          srfi-18
          sxml-transforms
          uri-common
          (only uri-generic uri-encode-string)          
          )

  ))

;;; HTML

(define (sxml->html doc #!optional port)
  (let* ((rules `((lit *preorder* . ,(lambda (t b) b))
                  . ,universal-conversion-rules*))
         (reply (lambda () (SRV:send-reply (pre-post-order* doc rules)))))
    (if port
        (with-output-to-port port reply)
        (with-output-to-string reply))))

(define (maybe pred x)
  (if pred x '()))

(define (charset c)
  (maybe c
         `(meta (@ (http-equiv "content-type")
                   (content "text/html; charset=" ,c)))))
(define (javascript u)
  (if (uri-reference? u)
      `(script (@ (type "text/javascript")
                  (src ,(uri->string (relative-uri u)))))
      `(script (@ (type "text/javascript"))
               (lit ,u))))
(define (css-link u)
  `(link (@ (rel stylesheet)
            (href ,(uri->string (relative-uri u)))
            (type "text/css"))))

;;; Pages

(define (->json x)           ;; q&d JSON generator since we don't need a parser.
  (define (quoted-string x)
    `("\"" ,(string-substitute (irregex '(: #\")) "\\\"" x)
      "\""))
  (define (alist->json a)
    (map (lambda (e) (list (quoted-string (symbol->string (car e)))
                      ":" (to-json (cdr e))))
         a))
  (define (to-json x)
    (cond ((number? x) (number->string x))
          ((pair? x) (list "{" (intersperse (alist->json x) ",") "}"))
          ((string? x) (quoted-string x))
          ((symbol? x) (quoted-string x))
          ((vector? x) (list "[" (intersperse (map ->json (vector->list x)) ",") "]"))
          (else (error 'json "conversion unsupported" x))))
  (tree->string (to-json x)))

(define (search-form)
  `(form (@ (class "lookup")
            (action ,(uri->string (relative-uri (uri-reference (cdoc-page-path)))))
            (method get))
         (input (@ (id "searchbox")
                   (class "text incsearch")
                   (data-opts ,(->json
                                `((url . ,(uri->string (relative-uri (incremental-search-uri))))
                                  (delay . ,(incremental-search-delay)))))
                   (type text)
                   (name q)
                   (autocomplete off) ;; apparently readonly in DOM
                   (autocorrect off)
                   (autocapitalize off)) ;; iphone/ipad
                )
         (div (@ (class "buttons"))
              (input (@ (class "button") (type submit)
                        (id "query-name") (name "query-name")
                        (value "Look up")))
              (input (@ (class "button") (type submit)
                        (id "query-regex") (name "query-regex")
                        (value "Regexp"))))))

(define (format-id x)
  (match (match-nodes x (maximum-match-results))
         ((n1)
          (redirect-to (path->href (node-path n1))))
         (()
          ;; Should we return 404 here?  This is not a real resource
          (node-page '()
                     '()
                     `(p "No node found matching identifier " (tt ,x))
                     page-title: "node not found"))
         (nodes
          (match-page nodes x))))

(define (format-re x)
  (match-page (match-nodes (irregex x) (maximum-match-results))
              x))
(define (format-path-re x)
  (match-page (match-node-paths/re (irregex x) (maximum-match-results))
              x))

(define (match-page nodes match-text)
  (let ((max-results (maximum-match-results))
        (result-length (length nodes)))
    (cache-for
     (cache-nodes-for) ;?
     (lambda ()
       (last-modified-at
        (max (repository-modification-time (current-repository))
             (last-modified))
        (lambda ()
          (node-page
           `("query " ,match-text " ("
             ;; Exceeding match-results can't happen because we cap the
             ;; result length now, but we keep the logic in place anyway
             ,(maybe (> result-length max-results)
                     `(,max-results " of "))
             ,result-length " matches)")
           '()                 ;contents
           (if (= result-length 0)
               '()
               `(table
                 (@ (class "match-results"))
                 (tr (th "path") (th "signature"))
                 ,(let loop ((sigs (maximum-match-signatures))
                             (results max-results)
                             (nodes nodes) (acc '()))
                    (if (or (null? nodes)
                            (<= results 0))
                        (reverse acc)
                        (let ((n (car nodes)))
                          (loop (- sigs 1) (- results 1)
                                (cdr nodes)
                                (cons
                                 `(tr
                                   (td (@ (class "match-path"))
                                       ,(title-path n))
                                   (td (@ (class "match-sig"))
                                       ,(path-link
                                         (node-path n)
                                         (if (<= sigs 0)
                                             "-"
                                             `(tt ,(node-signature n))))))
                                 acc)))))))
           page-title: "query results")))))))

(define (contents-list n)
  (let ((ids (node-child-ids n)))
    (if (null? ids)
        '()
        `((h2 (@ (class "contents-list"))
              "Contents " (& "raquo"))
          (ul (@ (class "contents-list"))
              ,(map
                (let ((child->href (make-child->href n)))
                  (lambda (id)
                    `(li
                      (a (@ (href ,(child->href id))) ,id))))
                (map ->string ids)))))))

(define (format-path p)
  (let ((n (handle-exceptions e #f (lookup-node (string-split p)))))
    (if n
        (cache-for   ;; NB We send cache-control even with 304s.
         (cache-nodes-for)
         (lambda ()
           (last-modified-at
            ;; Node modification time may also be more fine-grained,
            ;; but some generated HTML may depend on the entire repository
            ;; anyway--and we usually update the whole repo at once.
            (max (repository-modification-time (current-repository))
                 (last-modified))
            (lambda ()
              (if (null? (node-path n))
                  (node-page '()
                             (contents-list n)
                             (root-page))
                  (node-page (title-path n)
                             (contents-list n)
                             `(lit
                               . ,(chicken-doc-sxml->html (node-sxml n)
                                                          path->href
                                                          (make-def->href n)
                                                          man-filename->path))
                             page-title: (last (node-path n))))))))
        (node-not-found p `(p "No node found at path " (i ,p))))))

;; careful: redirect-to uses the output of this. Make sure it can handle
;; relative URIs, or make relative conversion optional.
(define (path->href p)             ; FIXME: use uri-relative-to, etc
  (define (path->uri-string p)
    (string-append
     (chickadee-page-path)
     "/"
     (string-intersperse (map (lambda (x)
                                (uri-encode-string (->string x)))
                              p) "/")))
  ;; We generate a relative path from the current request uri. This
  ;; won't be available if generating pages statically, but here we have
  ;; multiple ways to generate the same page (/doc/foo, /cdoc?q=foo),
  ;; match pages, &c. so it is hard to guess the entry URI.
  (let ((u (relative-uri (uri-reference (path->uri-string p)))))
    ;; (print "curreq: " (request-uri (current-request)))
    ;; (print "to: " u1 " from: " u2)
    (uri->string u)))

;; All invocations of relative-uri subsequently call uri->string, so
;; we could just add that here.
(define (relative-uri uri)
  (let ((current-uri
         (update-uri (uri-reference "")   ; strip all but path from request URI
                     path: (uri-path (request-uri (current-request))))))
    (uri-relative-from uri current-uri)))

;; Given a node N, return a procedure that will produce
;; an href for any child node ID of N.  Although simple now,
;; this could be extended to use relative paths when the current
;; URI permits it, saving some bandwidth.
(define (make-child->href n)
  (let ((path (node-path n)))
    (lambda (id)
      (if (node-definition-id? n id)
          (string-append "#" (quote-identifier (definition->identifier id)))
          (path->href (append path (list id)))))))

;; FIXME??? chg "identifier" to html-id (or maybe, fragment to html-id)

;; Given a node N, return a procedure that will produce a definition href
;; for ID suitable for placement in a defsig in N.  That is, it will refer
;; to the actual child node when N is an egg (etc.) and it will refer to
;; an anchor id in the parent when N is itself a defsig.
;; Returns #f if the ID is not a definition inside N. This is useful when
;; conditionally lighting up plaintext IDs as definition links. But when N
;; is a defsig, it does not check the parent.

(define (make-def->href n)
  (let ((doc (node-sxml n))
        (path (node-path n)))
    (if (eq? (car doc) 'def)
        (let* ((path (butlast path))
               (href (path->href path)))
          (lambda (id)
            (and (or (string=? (->string id)
                               (->string (node-id n))) ; Make sure to recognize ourselves!
                     (node-definition-id? n id)) ; Note: Does not look in parent. Possible fixme.
                 (string-append href "#" (quote-identifier (definition->identifier id))))))
        (lambda (id)
          (and (node-definition-id? n id)
               (path->href (append path (list id))))))))

(define (title-path n)
  (define (links n)
    (let loop ((p (node-path n))
               (f '())
               (r '()))
      (if (null? p)
          (reverse r)
          (let* ((id (->string (car p)))
                 (f (append f (list id)))
                 (n (lookup-node f)))
            (loop (cdr p) f (cons (path-link f id) r))))))
  
  (intersperse (links n)
               '(" " (& "raquo") " "))) ;; literal " &raquo; " would be nicer

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

(define (incremental-search-handler _)
  (with-request-vars*
   $ (q)
   ;; FIXME: doesn't skip 0 or #f incremental-search
   (let ((M (vector->list
             ((if (string-index q #\space)
                  match-paths/prefix
                  match-ids/prefix)
              q (incremental-search)))))
     (let ((body (if (null? M)
                     ""
                     (let ((plen (string-length q)))
                       (tree->string
                        `("<ul>"
                          ,(map (lambda (x)
                                  `("<li>"
                                    "<b>" ,(htmlize (substring x 0 plen))
                                    "</b>"
                                    ,(htmlize (substring x plen)) "</li>"))
                                M)
                          "</ul>"))))))
       ;; Latency pause for debugging
       (let ((pause (%chickadee:debug-incremental-search-latency)))
         (if (> pause 0)
             (thread-sleep! (/ pause 1000))))
       ;; Send last-modified headers? May not be worth it.
       (cache-privately-for  ; `private` has no effect on nginx proxy cache
        (cache-nodes-for)
        (lambda ()
          (parameterize ((access-log (ajax-log))) ; Logging is extremely slow
            (send-response body: body))))))))

;; Re matching, it might be more useful to match against each identifier level
;; with a separate regex.

(define (root-page)
  `((h3 "Search Chicken documentation")
    (p "Enter a node name or path in the search box below:")
    ,(search-form)
    (ul (li "A node name is an identifier, egg, module or unit name, such as "
            (i "open/rdonly") ", " (i "awful") ", "
            (i "scheme") " or " (i "eval") ".")
        (li "A node path is a sequence of node names, such as "
            (i "eval load") " or " (i "foreign types") ".")
        (li (i "Look up") " finds an exact match, and " (i "Regexp") " matches a regular expression.")
        (li "Regular expression matching is usually done against node names,"
            " but if a space is present, the full node path will be considered."))
    (h4 "Quick links")
    (ul (li ,(path-link '(chicken) "Chicken manual"))
        (li ,(path-link '(chicken language) "Supported language"))
        (li ,(path-link '(foreign) "FFI")))
    (h4 "About")
    (p ,(path-link '(chickadee))
       " is an interface to the "
       ,(path-link '(chicken-doc))
       " documentation system for "
       (a (@ (href "http://call-cc.org")) (span (@ (style "font-variant: small-caps")) "Chicken") " Scheme")
       ".")))

;; Conditionally add Internet Explorer classes to <html> a la HTML5 Boilerplate.
;; Also add 'no-js' (which we will change to 'js' if JS is enabled).
;; NOTE: you must supposedly add <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
;; or send that header to avoid Compatibility View in IE -- can't reproduce and
;; the <meta> solution is dubious, so not using this until can conduct more tests.
(define (conditional-html x)
  `((lit "
<!--[if lt IE 7]> <html class=\"no-js ie6 lt-ie9 lt-ie8 lt-ie7\"> <![endif]-->
<!--[if IE 7]>    <html class=\"no-js ie7 lt-ie9 lt-ie8\"> <![endif]-->
<!--[if IE 8]>    <html class=\"no-js ie8 lt-ie9\"> <![endif]-->
<!--[if gt IE 8]><!--> <html class=\"no-js\" lang=\"en\"> <!--<![endif]-->
"
    ) ,x (lit "</html>")))

;; Warning: TITLE, CONTENTS and BODY are expected to be HTML-quoted.
;; Internal fxn for node-page / not-found
(define (%node-page-body title contents body #!key (page-title #f))
  (sxml->html
   `((lit "<!doctype html>")
     (html (@ (class "no-js"))
      (head ,(charset "utf-8")
            ,(map css-link (chickadee-css-files))
            ,(map javascript (chickadee-early-js-files))
            ;; Remove "no-js" class and add "js" class to <HTML> when JS enabled a la Modernizr,
            ;; assuming Modernizr isn't already loaded.
            (script "if(!window.Modernizr){"
                    "this.document.documentElement.className="
                    "this.document.documentElement.className.replace(/\\bno-js\\b/,'js')"
                    "}")
            (title ,(if page-title
                        `(,page-title " | chickadee")
                        "chickadee server"))
            (meta (@ (name "viewport")
                     (content "initial-scale=1"))))
      (body
       ;; Removed navskip because #contents are now at bottom.  However, a TOC skip might be nice,
       ;; although you can click the next section instead.  Also, maybe provide a link to skip
       ;; to the #contents.
       ;; (p (@ (id "navskip"))
       ;;    (a (@ (href "#body")) "Skip navigation."))
       (div (@ (id "hdr"))
            (h1 ,(path-link '() "chickadee")
                ,(if (null? title)
                     `(;; (" | " ,(path-link '(chicken-doc)) " " "server"
                        )
                     `((lit " &raquo; ") ,title)))
            (h5 (label (@ (for "hdr-searchbox"))
                       "Identifier search"))
            (form (@ (id "hdr-lookup")
                     (class "hdr-lookup")
                     (action ,(uri->string (relative-uri (uri-reference (cdoc-page-path)))))
                     (method "get"))
                  (input (@ (id "hdr-searchbox")
                            (name "q")
                            (class "text incsearch")
                            (data-opts ,(->json
                                         `((url . ,(uri->string (relative-uri (incremental-search-uri))))
                                           (delay . ,(incremental-search-delay)))))
                            (type "text")
                            (accesskey "f")
                            (title "chickadee search (Ctrl-F)")
                            (autocomplete "off")
                            (autocorrect "off")
                            (autocapitalize "off")
                            (tabindex "1")))
                  (button (@ (id "hdr-submit") (name "query-name")
                             (title "Search chicken-doc for this identifier")
                             (class "button") (type "submit"))
                          (& "nbsp"))))
       (div (@ (id "body"))
            (div (@ (id "main"))
                 ,body))
       ,(maybe (not (null? contents))
               `(div (@ (id "contents"))
                     ,contents))
       ,(map javascript (chickadee-js-files)))))))

(define (node-page title contents body #!key (page-title #f))
  (send-response
   body: (%node-page-body title
                          contents
                          body
                          page-title: page-title)
   headers: `((content-type #(text/html ((charset . "utf-8"))))
              )))

(define (node-not-found title body)
  ;; Should create a dedicated not-found page instead;
  ;; but right now I don't want to duplicate main page code
  (send-response code: 404 reason: "Not found"
                 body:
                 (%node-page-body title
                                  '()
                                  body
                                  page-title: "node not found")))

(define cdoc-page-path (make-parameter #f))  ;; a string, not a URI -- redirect-to expects string
(define cdoc-uri
  (make-parameter (uri-reference "/cdoc")
                  (lambda (x)
                    (cdoc-page-path    ;; not relative--can't rely on current request being set
                     (and x (uri->string x)))
                    x)))
(define incremental-search-uri
  (make-parameter (uri-reference "/cdoc/ajax/prefix")))
(define incremental-search-delay     ; time in MS to delay incremental search requests
  (make-parameter 50))

(define chickadee-page-path (make-parameter #f)) ; cached -- probably not necessary
(define chickadee-uri
  (make-parameter (uri-reference "/chickadee")
                  (lambda (x)
                    (chickadee-page-path   ;auto update (mostly for debugging)
                     (and x (uri->string x)))
                    x)))

(define chickadee-css-files
  (make-parameter (list (uri-reference "/cdoc/chickadee.css"))))
(define chickadee-js-files
  (make-parameter (list (uri-reference "/cdoc/chickadee.js"))))
(define chickadee-early-js-files
  (make-parameter '()))

(define maximum-match-results (make-parameter 250))
(define maximum-match-signatures (make-parameter 100))
(define cache-nodes-for (make-parameter 300))
(define cache-static-content-for (make-parameter 3600))
;; Base time used for last-modified calculations, in seconds.
;; Set to (current)-seconds to invalidate pages when server is started.
(define last-modified (make-parameter 0))
;; Number of incremental search results to display; 0 or #f to disable.
(define incremental-search (make-parameter 0))
(define ajax-log (make-parameter #f)) ;; AJAX access log.  #f to disable.

;; debugging: incremental search latency, in ms
(define %chickadee:debug-incremental-search-latency (make-parameter 0))

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

(define (link href desc)
  `(a (@ (href ,href)) ,desc))
(define (path-link path #!optional desc)
  (link (path->href path)
        (or desc (string-intersperse (map ->string path) " "))))

(define ($ var #!optional converter/default)  ; from awful
    ((http-request-variables) var converter/default))
(define http-request-variables (make-parameter #f))

;; note: missing full node path should maybe generate 404
(define (redirect-to path #!key (code 302) (reason "Found") (headers '()))
  (send-response code: code
                 reason: reason
                 headers: `((location ,(uri-relative-to
                                        (uri-reference path)
                                        (request-uri (current-request)) ;; spiffy 4.8
                                        )))))

;; Return 304 Not Modified.  If ACTUAL-MTIME is an integer, it is
;; returned to the client as the actual modification time of the resource.
;; You should also resend any associated Cache-control: directive (separately).
(define (not-modified actual-mtime)
  (let ((headers (if (integer? actual-mtime)     ; error?
                     `((last-modified #(,(seconds->utc-time actual-mtime) ())))
                     '())))
    (send-response code: 304 reason: "Not modified" headers: headers)))

;; Compare mtime with If-Modified-Since: header in client request.
;; If newer, client's copy of resource is outdated and we execute thunk.
;; Otherwise, return 304 Not Modified.
(define (last-modified-at mtime thunk)
  (let ((header-mtime-vec (header-value
                           'if-modified-since
                           (request-headers (current-request)))))
    (if (or (not header-mtime-vec)
            (> mtime (utc-time->seconds header-mtime-vec)))
        (with-headers `((last-modified #(,(seconds->utc-time mtime) ())))
                      thunk)
        (not-modified mtime))))

;; SECONDS: number of seconds to cache for; or #t to set a far-future
;; expiration date (1 year as per RFC); or #f to force no caching.
(define (cache-for seconds thunk)
  (if (not seconds)
      (with-headers `((cache-control (max-age . 0)   ; use "no-cache" ?
                                     (must-revalidate . #t))) thunk)
      (let ((seconds (if (integer? seconds)
                         (min seconds 31536000)
                         31536000)))
        (with-headers `((cache-control (max-age . ,seconds))) thunk))))
(define (cache-privately-for seconds thunk)
  (if (not seconds)
      (with-headers `((x-accel-expires 0)  ; nginx hack
                      (cache-control (max-age . 0)
                                     (must-revalidate . #t)
                                     (private . #t))))
      (let ((seconds (if (integer? seconds)
                         (min seconds 31536000)
                         31536000)))
        (with-headers `((x-accel-expires 0) ; nginx hack
                        (cache-control (private . #t)
                                       (max-age . ,seconds)))
                      thunk))))

;; (define (uri-path->string p)   ; (/ "foo" "bar") -> "/foo/bar"
;;   (uri->string (update-uri (uri-reference "")
;;                            path: p)))

(define (proxy-logger)
  ;; access logger with X-Forwarded-For: header
  ;; Copied verbatim from spiffy's handle-access-logging
  (let ((h (request-headers (current-request))))
    (log-to (access-log)
            "~A ~A [~A] \"~A ~A HTTP/~A.~A\" ~A \"~A\" \"~A\""
            (header-value 'x-forwarded-for h "-")
            (remote-address)
            (seconds->string (current-seconds))
            (request-method (current-request))
            (uri->string (request-uri (current-request)))
            (request-major (current-request))
            (request-minor (current-request))
            (response-code (current-response))
            (uri->string (header-value 'referer h (uri-reference "-")))
            (let ((product (header-contents 'user-agent h)))
              (if product
                  (product-unparser product)   ; undocumented intarweb proc
                  "**Unknown product**")))))

;;;

(define (chickadee-handler p)
  (let ((path (string-intersperse p " ")))
    (format-path path)))

;; 'id' variable is never used externally nor is it generated in internal links.
;; 'q' and 'id' are identical, as long as there is no space present, and query-regex is not set.
(define (cdoc-handler p)
  p ;ignore
  (with-request-vars*
   $ (id path q)
     (cond (path => format-path)
           (id   => format-id)
           (q    => (lambda (p)
                      (with-request-vars*
                       $ (query-regex query-name)
                       (if query-regex
                           (if (string-index p #\space) ; hmm
                               (format-path-re p)
                               (format-re p))
                           (query p)))))
           (else (format-path "")))))

;;; handlers

;; vhost-map can be used to take control of requests as
;; they come in, before any handlers are invoked.

(define +vhost-map+
  `((".*" . ,(lambda (continue)
               (let ((p (uri-path (request-uri (current-request)))))
                 (parameterize ((http-request-variables (request-vars))) ; for $
                   (cond ((equal? (uri-path (cdoc-uri)) p)
                          => cdoc-handler)
                         ((match-path (uri-path (chickadee-uri)) p)
                          => chickadee-handler)
                         ((and (incremental-search-uri)
                               (equal? (uri-path (incremental-search-uri)) p))
                          => incremental-search-handler)
                         ;; Last resort redirect of root path to main page.
                         ((equal? p '(/ ""))
                          (redirect-to (path->href '())))
                         (else
                          (continue)))))))))

(define +not-found-handler+
 (let ((old-handler (handle-not-found)))  ; hmm
   (lambda (path) ; useless
     (old-handler path))))

(define +static-file-handler+
  (let ((old-handler (handle-file)))
    (lambda (path)
      (cache-for (cache-static-content-for)
                 (lambda ()
                   (old-handler path))))))

;;; start server

(define (chickadee-start-server)
  (verify-repository)
  ;; using parameterize, we cannot override in REPL
  (parameterize ((vhost-map +vhost-map+)
                 (handle-not-found +not-found-handler+)
                 (handle-file +static-file-handler+)
                 (handle-access-logging proxy-logger)
                 (tcp-buffer-size 1024)
                 (mime-type-map
                  `(("js" . application/x-javascript)  ;; spiffy has subpar mime-type
                    . ,(mime-type-map))))
    (start-server))))

;; time echo "GET /cdoc?q=p&query-regex=Regex HTTP/1.0" | nc localhost 8080 >/dev/null
;; (1374 matches) real    0m1.382s (warm cache) 
;;                real    0m1.098s (turn signatures off)
