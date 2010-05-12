(use chickadee spiffy)

(root-path "./root")
(debug-log (current-error-port))
(access-log "access.log")
(error-log "error.log")
(server-port 8383)
(max-connections 8)

(ajax-log #f)   ;; Slow; not recommended for production.
(cdoc-uri-path '(/ "cdoc"))
(chickadee-uri-path '(/ "chickadee"))
(incremental-search-uri-path '(/ "cdoc" "ajax" "prefix"))
(chickadee-css-path "/cdoc/chickadee.css")
(chickadee-js-path "/cdoc/chickadee.js?1")

(cache-static-content-for 1800)
(cache-nodes-for 600)

(maximum-match-results 250)
(maximum-match-signatures 100)
(incremental-search 15)

(last-modified (current-seconds))

(chickadee-start-server)
