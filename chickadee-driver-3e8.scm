(use chickadee spiffy uri-common)

(root-path "./root")
(debug-log (current-error-port))
(access-log "access.log")
(error-log "error.log")
(server-port 8383)
(max-connections 8)

(ajax-log #f)   ;; Slow; not recommended for production.
(define uri uri-reference)
(cdoc-uri (uri "/cdoc"))
(chickadee-uri (uri "/chickadee"))
(incremental-search-uri (uri "/cdoc/ajax/prefix"))
(chickadee-css-files (list (uri "/cdoc/chickadee.css")))
(chickadee-js-files (list (uri "/cdoc/chickadee.js?1")))

(cache-static-content-for 1800)
(cache-nodes-for 600)

(maximum-match-results 250)
(maximum-match-signatures 100)
(incremental-search 15)

(last-modified (current-seconds))

(chickadee-start-server)
