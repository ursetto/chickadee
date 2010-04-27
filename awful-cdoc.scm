;; todo
;;   rewrite /cdoc/path/to/node to ?path=path to node, somehow
;;   redir found ID to ?path=
;;   ----
;;   get rid of awful, probably
;;   bench

(use awful spiffy spiffy-request-vars html-tags html-utils chicken-doc)
(use matchable)
(use (only uri-generic uri-encode-string))
(use (only uri-common update-uri request-uri uri-reference))
(use (only intarweb request-uri))
(load "chicken-doc-html.scm") (import chicken-doc-html) ; temp -- for awful reload

(root-path ".")
(debug-log (current-error-port))

(page-exception-message
 (lambda (exn)
   (<pre> convert-to-entities?: #t
          (with-output-to-string
            (lambda ()
              (print-call-chain)
              (print-error-message exn))))))

(define (input-form)
  (<form> class: "lookup"
          action: (main-page-path)
          method: 'get
          (<input> class: "text" type: "text" name: "q")  ; query should redirect.
          (<input> class: "button" type: "submit" name: "query-name" value: "Lookup")
          (<input> class: "button" type: "submit" name: "query-regex" value: "Regex")))

;; Really needs to redirect to (or at least call) format-path
(define (format-id x)
  (match (match-nodes x)
         ((n1)
          (node-page (title-path n1)
                     (contents-list n1)
                     (chicken-doc-sxml->html (node-sxml n1))))
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
                                            (<tt> (node-signature n))))))
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
             "<a href=\"" ,(main-page-path) "?path="
             ,(uri-encode-string (string-intersperse
                                  (append p (list id))
                                  " "))
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
         (node-page p "" (<p> "No node found at path " p))))))

(define (path->href p)
  (string-append
   (main-page-path) "?path="
   (uri-encode-string (string-intersperse (map ->string p) " "))))
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
                            "<a href=\"" (main-page-path) "?path="
                            (uri-encode-string (string-intersperse f " "))
                            "\">" (quote-html id)
                            "</a>"
                            (if (null? (cdr p)) '() " &raquo; "))
                           r))))))

(define (query p)
  (let ((q (string-split p)))
    (cond ((null? q) (error "Query string missing")) 
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

(main-page-path "/cdoc")

(define-page (main-page-path)
  (lambda ()
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
            (node-page #f (contents-list (lookup-node '())) (root-page))))))

  no-template: #t)

(define (node-page title contents body)
  (html-page
   (++ (<h1> (link (main-page-path) "chickadee")
             (if title
                 (string-append " &raquo; " title)
                 (string-append " | chicken-doc server")))
       (<div> id: "contents"
              contents)
       (<div> id: "body"
              (<div> id: "main"
                     body)))
   css: "awful-cdoc.css"))

;; missing full node path should generate 404
;; "q" search should operate like chicken-doc cmd line

(define (redirect-to path #!key (status 302) (headers '()))
  (warning "redirecting to " path)
  (with-headers `((location ,(uri-reference (string-append "http://localhost:8080"
                                                           path))))
                (lambda ()
                  (send-status 302 "oh crap")
                  ""))
  ;; (send-response status: status
  ;;                headers: (append `((location ,(update-uri
  ;;                                               (request-uri (current-request))
  ;;                                               path: path)))
  ;;                                 headers))
  )
