(use chickadee spiffy)

(root-path "./root")
(debug-log (current-error-port))
(server-port 8080)

(cdoc-uri-path '(/ "cdoc"))
(chickadee-uri-path '(/ "chickadee"))
(chickadee-css-path "/cdoc/chickadee.css")

(chickadee-start-server)
