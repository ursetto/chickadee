;;; Chickadee chicken-doc server configuration file

;; chickadee changes to the directory holding the conf-file on startup,
;; so all paths below are relative to that path.

(use chickadee spiffy uri-common)   ;; REQUIRED.  Do not remove.
(define uri uri-reference)          ;; A convenient alias.

;;; Main config

(root-path "root")
(server-port 8080)

;; Logging.  If commented out, the defaults will be used:
;; access, debug and AJAX logs are disabled, and error logs go to stderr.

;(access-log "logs/access.log")
;(error-log "logs/error.log")
;(debug-log (current-error-port))
;(ajax-log "logs/ajax.log")         ;; CPU intensive; not recommended for production.

;; An even number of connections is desirable as user agents
;; usually open two (keep-alive) connections per hostname.
(max-connections 8)
(cdoc-uri (uri "/cdoc"))
(chickadee-uri (uri "/chickadee"))
(incremental-search-uri (uri "/cdoc/ajax/prefix"))
(chickadee-css-files (list (uri "/cdoc/chickadee.css")))
(chickadee-js-files (list (uri "http://code.jquery.com/jquery-1.4.2.min.js")
                          (uri "/cdoc/jquery.metadata.2.1.min.js")
                          (uri "/cdoc/chickadee-jquery.js")))

(maximum-match-results 250)
(maximum-match-signatures 100)
(incremental-search 15)
(incremental-search-delay 50)
(cache-nodes-for 300)
(cache-static-content-for 1800)

(last-modified (current-seconds))

;;; debugging

(%chickadee:debug-incremental-search-latency 0)

;;; start

