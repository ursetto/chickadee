(use chickadee spiffy)

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

(chickadee-start-server)
