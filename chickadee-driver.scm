(use chickadee spiffy posix)

(root-path "./root")
(debug-log (current-error-port))
(server-port 8080)
(access-log "access.log")

(cdoc-uri-path '(/ "cdoc"))
(chickadee-uri-path '(/ "chickadee"))
(chickadee-css-path "/cdoc/chickadee.css")

(maximum-match-results 250)
(maximum-match-signatures 100)
(cache-nodes-for 300)
(cache-static-content-for 1800)

(last-modified (current-seconds))

(chickadee-start-server)
