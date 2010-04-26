(use awful spiffy spiffy-request-vars html-tags html-utils chicken-doc)
(use matchable)
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
  (string-intersperse (map ->string (node-path n)) " &raquo; "))

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
  (++ (<h1> "chickadee"
            (if title (string-append " &raquo; " title) ""))
      (<div> id: "contents"
             contents)))
