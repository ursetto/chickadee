;; todo
;; Check if log is writable.  Spiffy will error out (but not until first log attempt).

(use chickadee spiffy uri-common posix)
(use matchable)

(define args (command-line-arguments))
(define +basedir+
  (make-pathname (chicken-home) "chickadee"))
(define +conf-file+
  (make-pathname +basedir+ "config.scm"))

(define (usage type)
  (define (usage-serve)
    (fprintf (current-error-port)
             "
 usage: ~A serve [options] [conf-file]
 
 Start a chicken-doc web server using the configuration file
 [conf-file] or the default configuration file,
 ~A.
 If provided, [options] override settings in [conf-file].
 
 options:
 
  -p --port NUM         Port number
  
  -A --access-log FILE  Access log file
  -E --error-log FILE   Error log file     
  -D --debug-log FILE   Debug log file
  -J --ajax-log FILE    AJAX access log
    FILE is a filename relative to the directory holding [conf-file].
    It may be \"-\" to log to the console or \"\" to suppress logging.
 
 "
           (program-name)
           +conf-file+))
  (define (usage-help)
    (fprintf (current-error-port)
             "
 usage: ~A command args...

 commands:

   serve               Start a chicken-doc webserver
   location            Show location of datafiles
   help                Get help on command

 "
             (program-name)))
  (define (usage-location)
    (fprintf (current-error-port)
             "
 usage: ~A location

 Prints information about the location of important chickadee
 data files, such as the base directory and config file.
 
 "
             (program-name)))
  (cond ((equal? type "serve")
         (usage-serve))
        ((equal? type "help")
         (usage-help))
        ((equal? type "location")
         (usage-location))
        (else (usage "help")))
  ;(exit 1)
  )

(match args
       (("serve" . args)
        (let collect ((alog #f)
                      (elog #f)
                      (dlog #f)
                      (jlog #f)                      
                      (port #f)
                      (file +conf-file+)
                      (args args))
          (cond ((pair? args)
                 (let ((a (car args)))
                   (cond ((or (equal? a "--access-log")
                              (equal? a "-A"))
                          (collect (cadr args) elog dlog jlog port file (cddr args)))
                         ((or (equal? a "--error-log")
                              (equal? a "-E"))
                          (collect alog (cadr args) dlog jlog port file (cddr args)))
                         ((or (equal? a "--debug-log")
                              (equal? a "-D"))
                          (collect alog elog (cadr args) jlog port file (cddr args)))
                         ((or (equal? a "--ajax-log")
                              (equal? a "-J"))
                          (collect alog elog dlog (cadr args) port file (cddr args)))
                         ((or (equal? a "--port")
                              (equal? a "-p"))
                          (collect alog elog dlog jlog
                                    (string->number (cadr args))
                                    file (cddr args)))
                         (else
                          (if (null? (cdr args))
                              (collect alog elog dlog jlog port a (cdr args))
                              (usage "serve"))))))
                (else
                 (cond ((pathname-directory file)
                        => change-directory))
                 (load file)
                 (when port (server-port port))
                 (when alog
                   (access-log
                    (cond ((equal? alog "-") (current-output-port))
                          ((equal? alog "") #f)
                          (else alog))))
                 (when jlog
                   (ajax-log
                    (cond ((equal? jlog "-") (current-output-port))
                          ((equal? jlog "") #f)
                          (else jlog))))
                 (when elog
                   (error-log
                    (cond ((equal? elog "-") (current-error-port))
                          ((equal? elog "") #f)
                          (else elog))))
                 (when dlog
                   (debug-log
                    (cond ((equal? dlog "-") (current-error-port))
                          ((equal? dlog "") #f)
                          (else dlog))))
                 (fprintf (current-error-port)
                          "chickadee server listening on :~a\n" (server-port))
                 (chickadee-start-server)))

         ))
       (("location")  ;; accept conf file?
        (print "conf: " +conf-file+)
        (print "base: " +basedir+))
       (("help" cmd)
        (usage cmd))
       (else (usage "help")))



