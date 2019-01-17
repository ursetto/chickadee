;;; Chickadee chicken-doc server configuration file

;; chickadee changes to the directory holding the conf-file on startup,
;; so all paths below are relative to that path.

(cond-expand
 (chicken-4
  (use chickadee spiffy uri-common)      ;; REQUIRED.  Do not remove.
  (use (only chicken-doc-html syntax-highlighter colorize prettify-js)))
 (else
  (import chickadee spiffy uri-common    ;; REQUIRED.  Do not remove.
          (only chicken-doc-html syntax-highlighter colorize prettify-js)
          (chicken time))))

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
(cdoc-uri (uri "/cdoc/"))
(chickadee-uri (uri "/chickadee"))
(incremental-search-uri (uri "/cdoc/ajax/prefix"))
(chickadee-css-files (list (uri "/cdoc/chickadee.css")))
(chickadee-early-js-files (list (uri "/cdoc/modernizr.respond.93248.js")))
(chickadee-js-files (list (uri "/cdoc/jquery-1.9.0.min.js")  ;; Use local jquery only.
                          (uri "/cdoc/chickadee-jquery.js")
                          ;(uri "/cdoc/prettify-bundle.js")  ;; Uncomment if using prettify-js highlighter.
                          ))

(maximum-match-results 250)
(maximum-match-signatures 250)
(incremental-search 15)
(incremental-search-delay 50)
(cache-nodes-for 300)
(cache-static-content-for 1800)

;; (syntax-highlighter prettify-js)   ;; Uncomment to use prettify-js instead of colorize

(last-modified (current-seconds))

;;; debugging

(%chickadee:debug-incremental-search-latency 0)

;;; start

