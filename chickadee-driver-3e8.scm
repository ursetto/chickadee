(use chickadee spiffy uri-common)
(define uri uri-reference)

(root-path "./root")
(debug-log (current-error-port))
(access-log "access.log")
(error-log "error.log")
(server-port 8388)
(max-connections 8)

(ajax-log #f)   ;; Slow; not recommended for production.
(cdoc-uri (uri "/cdoc"))
(chickadee-uri (uri "/doc"))
(incremental-search-uri (uri "/cdoc/ajax/prefix"))
(chickadee-css-files (list (uri "/cdoc/chickadee.css?17")))
(chickadee-early-js-files (list (uri "/cdoc/modernizr.custom.93248.js")))
(chickadee-js-files (list (uri "http://code.jquery.com/jquery-1.4.2.min.js")
                          (uri "/cdoc/jquery.metadata.2.1.min.js")
                          (uri "/cdoc/chickadee-jquery.js?7")))

(maximum-match-results 250)
(maximum-match-signatures 100)
(incremental-search 15)
(incremental-search-delay 50)
(cache-nodes-for 600)
(cache-static-content-for 1800)

(last-modified (current-seconds))

;;; start

(chickadee-start-server)
