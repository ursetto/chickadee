(use awful spiffy spiffy-request-vars html-tags html-utils chicken-doc)
(use matchable)
(use (only uri-generic uri-encode-string))
(load "chicken-doc-html.scm")
(import chicken-doc-html)

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
          (<input> type: "text" name: "id")
          (<input> type: "submit")))

(define (format-doc x)
  (match (match-nodes x)
         ((n1)
          (node-page (title-path n1)
                     (chicken-doc-sxml->html (node-sxml n1))))
         (nodes
          (node-page x
                     (<pre> "Found " (length nodes) " nodes"
                            " at paths " (map node-path nodes))))))

(define (format-path p)
  (let ((n (handle-exceptions e #f (lookup-node (string-split p)))))
    (if n
        (node-page (title-path n)
                   (chicken-doc-sxml->html (node-sxml n)))
        (node-page p (<p> "No node found at path " p)))))

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
                            "\">" id
                            "</a>"
                            (if (null? (cdr p)) '() " &raquo; "))
                           r))))))

(define-page "cdoc"
  (lambda ()
    (with-request-vars
     $ (id path)
     (cond (path => format-path)
           (id   => format-doc)
           (else (node-page "chicken-doc server"
                            (input-form))))))

  css: "awful-cdoc.css")

(define (node-page title contents)
  (++ (<h1> "<a href=\"/cdoc\">chickadee</a>"
            (if title (string-append " &raquo; " title) ""))
      (<div> id: "contents"
             contents)))

;; missing full node path should generate 404
;; "q" search should operate like chicken-doc cmd line
