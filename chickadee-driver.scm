(use chickadee spiffy posix uri-common)
(define uri uri-reference)

(root-path "./root")
(debug-log (current-error-port))
(server-port 8080)
(access-log "access.log")

;; Note: if connections is small and AJAX parallel requests
;; are allowed, spiffy may take tens of seconds to respond,
;; occasionally.  May be an issue with keepalives.

;; An even number of connections is desirable as user agents
;; usually open two (keep-alive) connections per hostname.
(max-connections 4)

(ajax-log "/tmp/ajax.log")
(cdoc-uri (uri "/cdoc"))
(chickadee-uri (uri "/chickadee"))
(incremental-search-uri (uri "/cdoc/ajax/prefix"))
(chickadee-css-files (list (uri "/cdoc/chickadee.css")))
(chickadee-js-files (list (uri "/cdoc/chickadee.js")))

(maximum-match-results 250)
(maximum-match-signatures 100)
(incremental-search 15)
(cache-nodes-for 300)
(cache-static-content-for 1800)

(last-modified (current-seconds))

;;; debugging

(%chickadee:debug-incremental-search-latency 0)

;;; start

(chickadee-start-server)
