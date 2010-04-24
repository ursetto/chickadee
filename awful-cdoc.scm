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
  (<form> action: (main-page-path)
          method: 'post
          (<input> type: "text" name: "x")
          (<input> type: "submit")))

(define (format-doc x)
  (match (match-nodes x)
         ((n1)
          (chicken-doc-sxml->html (node-sxml n1)))
         (nodes
          (<pre> "Found " (length nodes) " nodes"
                 " at paths " (map node-path nodes)))))
  
(define-page (main-page-path)
  (lambda ()
    (with-request-vars $ (x)
      (if x
          (format-doc x)
          (input-form)
          )))
  css: "awful-cdoc.css")
