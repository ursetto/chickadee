(use awful spiffy spiffy-request-vars html-tags html-utils chicken-doc)
(use matchable)
(use (only uri-generic uri-encode-string))
(use chicken-doc-html)

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
  (<form> action: "cdoc"
          method: 'get
          (<input> type: "text" name: "q")  ; query should redirect.
          (<input> type: "submit" name: "query-name" value: "Lookup")
          (<input> type: "submit" name: "query-regex" value: "Regex")))

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
             "<a href=\"/cdoc?path="
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
   "/cdoc?path="
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
                            "<a href=\"/cdoc?path="
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
           (format-path p)))))                 ;  API defect

(define-page "cdoc"
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
            (node-page #f (contents-list (lookup-node '())) (input-form))))))

  css: "awful-cdoc.css")

(define (node-page title contents body)
  (++ (<h1> "<a href=\"/cdoc\">chickadee</a>"
            (if title
                (string-append " &raquo; " title)
                (string-append " | chicken-doc server")))
      (<div> id: "contents"
             contents)
      (<div> id: "body"
             (<div> id: "main"
                    body))))

;; missing full node path should generate 404
;; "q" search should operate like chicken-doc cmd line
