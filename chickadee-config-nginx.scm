(use chickadee spiffy uri-common simple-sha1)

;; Helpers
(define uri uri-reference)
(define (fingerprint fn)
  ;; alternative: (number->string (file-modification-time fn))
  (substring (or (sha1sum fn) (error "file not found" fn))
             0 8))
(define cache-bust ;; Fingerprint FN (relative to cdoc) and return cache-busting URI.
  (lambda (fn)
    (let ((cdoc (uri->string (cdoc-uri))))
      (uri (string-append cdoc "/" fn "?"
                          (fingerprint (make-pathname (list (root-path) cdoc)
                                                      fn)))))))

(root-path "root")
(server-port 8388)
(max-connections 8)

(access-log "logs/access.log")
(error-log "logs/error.log")
(debug-log (current-error-port))
(ajax-log #f)   ;; Slow; not recommended for production.

(cdoc-uri (uri "/cdoc"))
(chickadee-uri (uri "/doc"))
(incremental-search-uri (uri "/cdoc/ajax/prefix"))

(chickadee-css-files (list (cache-bust "chickadee.css")))
(chickadee-early-js-files (list (uri "/cdoc/modernizr.respond.93248.js")))
(chickadee-js-files (list (uri "http://code.jquery.com/jquery-1.4.2.min.js")
                          (uri "/cdoc/jquery.metadata.2.1.min.js")
                          (cache-bust "chickadee-jquery.js")))

(maximum-match-results 250)
(maximum-match-signatures 100)
(incremental-search 15)
(incremental-search-delay 50)
(cache-nodes-for 600)
(cache-static-content-for #t)

(last-modified (current-seconds))
