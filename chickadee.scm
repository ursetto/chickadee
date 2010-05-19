;; chickadee chicken-doc server
;; Copyright (c) 2010 Jim Ursetto.  All Rights Reserved.
;; License: BSD.

(module chickadee
 (chickadee-start-server
  cdoc-uri
  chickadee-uri
  incremental-search-uri
  chickadee-css-files
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

(import scheme chicken)
(import tcp data-structures srfi-1)
(use spiffy-request-vars html-tags html-utils chicken-doc)
(use spiffy)
(use matchable)
(use (only uri-generic uri-encode-string))
(use uri-common)
(use intarweb)
;(load "chicken-doc-html.scm")
(use chicken-doc-html)
(use doctype)
(use regex) (import irregex)
(use (only srfi-13 string-index string-concatenate))
(use (only posix seconds->string seconds->utc-time utc-time->seconds))
(use srfi-18)

;;; Pages

(define (input-form)
  (<form> class: "lookup"
          action: (cdoc-page-path)
          method: 'get
          (<input> id: "searchbox" class: (string-append "text incsearch { "
                                                         "url: \"" (uri->string (incremental-search-uri)) "\","
                                                         "delay: " (number->string (incremental-search-delay))
                                                         " }")
                   type: "text" name: "q"
                   autocomplete: "off"  ;; apparently readonly in DOM
                   autocorrect: "off" autocapitalize: "off" ;; iphone/ipad
                   )
          (<input> class: "button" type: "submit" id: "query-name" name: "query-name" value: "Lookup")
          (<input> class: "button" type: "submit" id: "query-regex" name: "query-regex" value: "Regex")))

(define (format-id x)
  (match (match-nodes x)
         ((n1)
          (redirect-to (path->href (node-path n1))))
         (()
          ;; Should we return 404 here?  This is not a real resource
          (node-page #f
                     ""
                     (<p> "No node found matching identifier " (<tt> (htmlize x)))))
         (nodes
          (match-page nodes x))))

(define (format-re x)
  (match-page (match-nodes (irregex x)) x))
(define (format-path-re x)
  (match-page (match-node-paths/re (irregex x)) x))

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
           (string-append "query " match-text " ("
                          (if (> result-length max-results)
                              (string-append (number->string
                                              max-results) " of ")
                              "")
                          (number->string result-length)
                          " matches)")
           ""                 ;contents
           (if (= result-length 0)
               ""
               (tree->string
                (list
                 "<table class=\"match-results\">"
                 (<tr> (<th> "path") (<th> "signature"))
                 (let loop ((sigs (maximum-match-signatures))
                            (results max-results)
                            (nodes nodes) (acc '()))
                   (if (or (null? nodes)
                           (<= results 0))
                       (reverse acc)
                       (let ((n (car nodes)))
                         (loop (- sigs 1) (- results 1)
                               (cdr nodes)
                               (cons 
                                (list
                                 "<tr>"
                                 (<td> class: "match-path" (title-path n))
                                 (<td> class: "match-sig"
                                       (<a> href: (path->href (node-path n))
                                            (if (<= sigs 0)
                                                "-"
                                                (<tt> convert-to-entities?: #t
                                                      (node-signature n)))))
                                 "</tr>")
                                acc)))))
                 "</table>"))))))))))

(define (contents-list n)
  (let ((p (map ->string (node-path n)))
        (ids (node-child-ids n)))
    (if (null? ids)
        ""
        (tree->string
         `("<h2 class=\"contents-list\">Contents</h2>\n"
           "<ul class=\"contents-list\">"
           ,(map
             (lambda (id)
               `("<li>"
                 "<a href=\"" ,(path->href (append p (list id)))
                 "\">" ,(quote-html id)
                 "</a>"
                 "</li>"))
             (map ->string ids))
           "</ul>\n"
           )))))

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
                  (node-page #f
                             (contents-list n)
                             (root-page))
                  (node-page (title-path n)
                             (contents-list n)
                             (chicken-doc-sxml->html (node-sxml n)
                                                     path->href)))))))
        (node-not-found p (<p> "No node found at path " (<i> (htmlize p)))))))

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
          ;; (send-response
          ;;   body: body)              
          (parameterize ((access-log (ajax-log))) ; Logging is extremely slow
            (send-response body: body))))))))

(define (root-page)
  (++ (<h3> "Search")
      (input-form)
      (<p> "Enter a documentation node name or path in the search box above."
           (<ul> (<li> "A node name is a single word, usually an identifier or egg name.  Examples: "
                       (<tt> (<u> "posix")) ", " (<tt> (<u> "open/rdonly"))
                       ", " (<tt> (<u> "+")) ".")
                 (<li> "A node path is multiple words, separated by spaces, such as "
                       (<tt> (<u> "posix open/rdonly")) ".")
                 (<li> "Regular expression search is usually done on single identifiers, but if it contains a space, the match is against the full path.")
                 ;; It might be more useful to match against each identifier level
                 ;; with a separate regex.
                 ))

      (<h3> "Quick links")
      (<ul> (<li> (<a> href: (path->href '(chicken)) "Chicken manual"))
            (<li> (<a> href: (path->href '(chicken language)) "Supported language"))
            (<li> (<a> href: (path->href '(foreign)) "FFI"))
                  )))

;; Warning: TITLE, CONTENTS and BODY are expected to be HTML-quoted.
;; Internal fxn for node-page / not-found
(define (%node-page-body title contents body)
  (html-page
   (++ (<p> id: 'navskip
            (<a> href: "#body" "Skip navigation."))
       (<div> id: "hdr"
              (<h1> (link (path->href '()) "chickadee")
                    (if title
                        (string-append " &raquo; " title)
                        (string-append " | "
                                       (link (path->href '(chicken-doc))
                                             "chicken-doc")
                                       " server")))
              (<form> id: "hdr-lookup"
                      class: "hdr-lookup"
                      action: (cdoc-page-path)
                      method: 'get
                      (<input> id: "hdr-searchbox" name: "q"
                               class: (string-append
                                       "text incsearch { "
                                       "url: \"" (uri->string (incremental-search-uri)) "\","
                                       "delay: " (number->string (incremental-search-delay)) " }")
                               type: "text"
                               autocomplete: "off" autocorrect: "off"
                               autocapitalize: "off"
                               tabindex: "1")
                      ;; fixme: add "title" attr, accesskey
                      ;; fixme: change this to a "button"
                      (<input> id: "hdr-submit" name: "query-name" value: "Lookup"
                               class: "button"
                               type: "submit"
                               tabindex: "2")))
       (<div> id: "contents"
              contents)
       (<div> id: "body"
              (<div> id: "main"
                     body)))
   headers: (string-concatenate         ;; Note: cacheable
             (map (lambda (x) (<script> type: "text/javascript" src: x))
                  (map uri->string (chickadee-js-files))))
   css: (map uri->string (chickadee-css-files))
   charset: "UTF-8"
   doctype: xhtml-1.0-strict
   ;; no good way to get a nice title yet
   title: "chickadee | chicken-doc server"))

(define (node-page title contents body)
  (send-response
   body: (%node-page-body title contents body)
   headers: `((content-type #(text/html ((charset . "utf-8"))))
              )))

(define (node-not-found title body)
  ;; Should create a dedicated not-found page instead;
  ;; but right now I don't want to duplicate main page code
  (send-response code: 404 reason: "Not found"
                 body:
                 (%node-page-body (htmlize title)  ; quoting critical
                                  "" body)))

(define cdoc-page-path (make-parameter #f)) ; cached -- probably not necessary
(define cdoc-uri
  (make-parameter (uri-reference "/cdoc")
                  (lambda (x)
                    (cdoc-page-path
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

(define ++ string-append)  ; legacy from awful
(define (link href desc)
  (<a> href: href desc))
(define ($ var #!optional converter/default)  ; from awful
    ((http-request-variables) var converter/default))
(define http-request-variables (make-parameter #f))

;; note: missing full node path should maybe generate 404
(define (redirect-to path #!key (code 302) (reason "Found") (headers '()))
  (send-response code: code
                 reason: reason
                 headers: `((location ,(uri-relative-to
                                        (uri-reference path)
                                        (server-root-uri))))))

;; Return 304 Not Modified.  If ACTUAL-MTIME is an integer, it is
;; returned to the client as the actual modification time of the resource.
;; You should also resend any associated Cache-control: directive (separately).
(define (not-modified actual-mtime)
  (let ((headers (if (integer? actual-mtime)     ; error?
                     `((last-modified #(,(seconds->utc-time actual-mtime))))
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
        (with-headers `((last-modified #(,(seconds->utc-time mtime))))
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

(define (rewrite-chickadee-uri u p)
  (let ((q (uri-query u)))
    (update-uri u
                path: (uri-path (cdoc-uri))
                query: (if (null? p)
                           q
                           (update-param 'path
                                         (string-intersperse p " ") q)))))

(define (chickadee-handler p)
  (rewrite-uri (lambda (u) (rewrite-chickadee-uri u p)))) ;?

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
