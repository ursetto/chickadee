(use awful spiffy spiffy-request-vars html-tags html-utils chicken-doc)

(root-path ".")
(debug-log (current-error-port))

(page-exception-message
 (lambda (exn)
   (<pre> convert-to-entities?: #t
          (with-output-to-string
            (lambda ()
              (print-call-chain)
              (print-error-message exn))))))

(define (link-egg egg)
  (<a> href: (conc "http://chicken.wiki.br/eggref/4/" egg) egg))


(define (format-line line)
  (string-substitute "`([^`]*)`" (<span> class: "code" "\\1") line #t)) ;; `code`
  
(define (format-description lines)
  (let ((listing? #f))
    (map (lambda (line)
           (set! line (format-line (htmlize (string-trim-both line))))
           (cond ((string-prefix? "*" line)
                  (++ (if listing? "" "<ul>")
                      (<li> line)))
                 ((string-prefix? "(" line)
                  (<pre> line))
                 ((string-null? line)
                  "<br>")
                 (else
                  (++ (if listing?
                          (begin
                            (set! listing? #f)
                            "</ul>")
                          "")
                      line))))
         lines)))

(define (format-prototype proto)
  (string-substitute "\\(([^ ]*)(.*)"
                     (++ "(" (<span> class: "operator" "\\1") "\\2")
                     proto 1))
  
(define (format-doc x)
  (let* ((lines (with-input-from-string
                    (with-output-to-string
                      (lambda ()
                        (doc-dwim (string->symbol x))))
                  read-lines))
         (path (with-input-from-string (cadr (string-split (car lines) ":")) read))
         (egg (car path))
         (identifier (cadr path))
         (prototype-tokens (string-split (caddr lines) ":"))
         (type (car prototype-tokens))
         (prototype (string-intersperse (cdr prototype-tokens) ":"))
         (description (string-intersperse (format-description (cdddr lines)))))
    (++ (<div> class: "doc-header"
               "Documentation for " (<span> convert-to-entities?: #t identifier)
               " from " (link-egg egg))
        (<div> class: "prototype" type ": " ;(<code> convert-to-entities?: #t prototype))
               (format-prototype (htmlize prototype)))
        (<div> class: "description" description))))

(define (input-form)
  (<form> action: (main-page-path)
          method: 'post
          (<input> type: "text" name: "x")
          (<input> type: "submit")))
  
(define-page (main-page-path)
  (lambda ()
    (with-request-vars $ (x)
      (if x
          (format-doc x)
          (input-form)
          )))
  css: "mario-doc.css")
