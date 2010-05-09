(use chickadee spiffy posix)

(root-path "./root")
(debug-log (current-error-port))
(server-port 8080)
(access-log "access.log")
;; Note: if connections is small and AJAX parallel requests
;; are allowed, spiffy may take tens of seconds to respond,
;; occasionally.  May be an issue with keepalives.
(max-connections 2)

(cdoc-uri-path '(/ "cdoc"))
(chickadee-uri-path '(/ "chickadee"))
(chickadee-css-path "/cdoc/chickadee.css")
(chickadee-js-path "/cdoc/chickadee.js")

(maximum-match-results 250)
(maximum-match-signatures 100)
(incremental-search 15)
(cache-nodes-for 300)
(cache-static-content-for 1800)

(last-modified (current-seconds))

(chickadee-start-server)
