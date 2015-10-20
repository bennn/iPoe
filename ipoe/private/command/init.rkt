#lang racket/base

;; Start a fresh ipoe database

(provide
  init
)

;; -----------------------------------------------------------------------------

(require
  racket/cmdline
  ipoe/private/db
  ipoe/private/parameters
  ipoe/private/ui
  (only-in racket/system system)
)

;; =============================================================================

(define *new-user* (make-parameter #f))
(define *new-dbname* (make-parameter #f))

(define (init arg*)
  ;; TODO help string (link to documentation)
  (command-line
   #:argv arg*
   #:once-each
   [("-u" "--user")
    u-param "Username for new database" (*new-user* u-param)]
   [("-d" "--dbname")
    d-param "Name of new database" (*new-dbname* d-param)]
   #:args ()
   (and
     (start-server)
     (parameterize-from-hash (options-init)
       (lambda ()
         (define u (get-username (*new-user*) (*user*)))
         (define d (get-dbname   (*new-dbname*) (*dbname*)))
         (and
           (unless (psql-user-exists? u)
             (psql-create-user u))
           (unless (psql-db-exists? u d)
             (psql-create-db u d))
           (unless (psql-tables-exist? u d)
             (psql-create-tables u d))
           (unless (and (*user*) (*dbname*))
             (save-config u d))))))))

;; -----------------------------------------------------------------------------

(define-syntax-rule (init-error msg arg* ...)
  (raise-user-error 'ipoe:init (format msg arg* ...)))

;; -----------------------------------------------------------------------------

(define U-PROMPT "Enter a username for the new database.")

;; Do a fallback search to get a username
;; (: get-username (-> (U #f String) (U #f String) String))
(define (get-username u-cmdline u-config)
  ;; First, try the commandline or the current user
  (define u-first
    (or u-cmdline
        (let ([sys-user (getenv "USER")])
          (case (get-user-input read-yes-or-no
                #:prompt (format "Enter 'Y' to create a database owned by user '~a'" sys-user)
                #:description (format "No user specified on command line. Do you want to create a database for the current system user '~a'?" sys-user))
           [(Y) sys-user]
           [else #f]))))
  (param-fallback u-first u-config
                  #:src 'username
                  #:prompt U-PROMPT))

(define DB-PROMPT "Enter a name for the new database")

;; Do a fallback search to get a database name
;; (: get-dbname (-> (U #f String) (U #f String) String))
(define (get-dbname d-cmdline d-config)
  (param-fallback d-cmdline d-config
                  #:src 'database
                  #:prompt DB-PROMPT))

(define (param-fallback p1 p2 #:src src
                              #:prompt prompt
                              #:descr [descr #f])
  (define p
    (cond
     [p1    (alert (format "Got ~a '~a' from command-line" src p1)) p1]
     [p2    (alert (format "Inferred ~a '~a' from config file" src p2)) p2]
     [else  (get-user-input read-sql-id
                            #:prompt prompt
                            #:description descr)]))
  (or (read-sql-id p)
      (init-error "Invalid ~a '~a', must be a SQL identifier (lowercase alphanumeric, underscores permitted)" src p)))

;; Check if the user exists, and if not print an error telling how to create it
;; (2015-09-21: could try to create user here like the name suggests, but
;;              we do need a superuser ...)
(define (psql-create-user user)
  (define cmd (format "createuser -d ~a" user))
  (init-error "User '~a' does not exist. Run the command '~a' as a superuser and try again." user cmd))

(define (psql-user-exists? user)
  (alert (format "Checking that user '~a' is recognized by psql ..." user))
  (system (format "psql -U ~a -l > /dev/null 2>&1" user)))

(define (psql-db-exists? user dbname)
  (alert (format "Checking that user '~a' has access to psql database '~a' ..." user dbname))
  (system (format "psql -U ~a -d ~a -l > /dev/null 2>&1" user dbname)))

(define (psql-tables-exist? user dbname)
  (alert (format "Checking that 'word' database exists ..."))
  (not (system (format "psql -U ~a -d ~a -c \"SELECT relname FROM pg_class WHERE relname='word';\" | grep -q \"0 rows\"" user dbname))))

(define DESCRIPTION "DB for the Interactive Poetry Editor (ipoe)")
(define (psql-create-db user dbname)
  (define cmd (format "createdb -e -O ~a ~a '~a'" user dbname DESCRIPTION))
  (alert (format "Creating database '~a' for user '~a' ..." dbname user))
  (system cmd))

(define (psql-create-tables user dbname)
  (alert "Creating iPoe tables ...")
  (with-ipoe-db #:commit? #t
                #:user user
                #:dbname dbname
    create-ipoe-tables))

(define (psql-installed?)
  (alert "Checking for `psql` command ...")
  (or
    (system "type psql >/dev/null 2>&1")
    (and (alert "Cannot find `psql` command, please install postgres and try again.\n    https://wiki.postgresql.org/wiki/Detailed_installation_guides")
         #f)))

(define (psql-running?)
  (alert "Checking for psql server ...")
  ;;bg; My own hack, not sure if robust
  (system "psql -l >/dev/null 2>&1"))

(define (save-config user dbname)
  (alert "Successfully created ipoe database.")
  (define-values [gc lc] (get-config-filenames))
  (case (get-user-input read-yes-or-no
                        #:prompt (format "Save current user/dbname to the global config file ('~a')?" gc))
   [(Y)
    (update-option 'user user)
    (update-option 'dbname dbname)]
   [else (void)]))

(define (start-server)
  (and (psql-installed?)
       (or (psql-running?)
           ;; Heeeere we go!
           (and (alert "No psql server found, starting a new server ...")
                (system "su postgres -c 'pg_ctl start -D /var/lib/postgres/data -l /tmp/PGSQL.log'")))))

;; =============================================================================

(module+ test
  (require rackunit ipoe/private/util/rackunit-abbrevs)

  ;; -- get-username
  (check-apply* (lambda (k1 k2)
                  (check-print (list #rx"command-line$")
                    (lambda () (get-username k1 k2))))
   ["foo" #f == "foo"]
   ["foo" "bar" == "foo"])

  ;; -- get-dbname
  (check-apply* (lambda (k1 k2)
                  (check-print (list #rx"command-line$")
                    (lambda () (get-username k1 k2))))
   ["foo" #f == "foo"]
   ["foo" "bar" == "foo"])

  ;; -- param-fallback
  (check-equal?
    (check-print (list #rx"command-line$")
      (lambda () (param-fallback "yes" #f #:src #f #:prompt #f #:descr #f)))
    "yes")

  (check-equal?
    (check-print (list #rx"config file$")
      (lambda () (param-fallback #f "yes" #:src #f #:prompt #f #:descr #f)))
    "yes")

  (define-syntax-rule (check-bad-param-0 p)
    (check-exn #rx"ipoe:init"
      (lambda ()
        (check-print (list #rx"^Got")
          (lambda ()
            (param-fallback p #f #:src 'bad-param1 #:prompt "heyo" #:descr "bye"))))))
  (define-syntax-rule (check-bad-param-1 p)
    (check-exn #rx"ipoe:init"
      (lambda ()
        (check-print (list #rx"^Got")
          (lambda ()
            (param-fallback #f p #:src 'bad-param1 #:prompt "heyo" #:descr "bye"))))))

  (for ([bp (in-list '(yolo "54" "A1" "1_" "hello-there" "can't use this" "YO LO"))])
    (check-bad-param-0 bp)
    (check-bad-param-1 bp))

  ;; -- psql-create-user, failure
  (check-exn #rx"ipoe:init"
    (lambda ()
      (check-print (list #rx"^Checking that user")
        (lambda () (psql-create-user "FAKE-USER")))))

  ;; -- psql-create-user, success
  ;(parameterize-from-hash (options-init)
  ;  (lambda ()
  ;    (define u (*user*))
  ;    (cond
  ;     [u
  ;      ;; User exists, let's try the test
  ;      (check-equal?
  ;        (check-print (list #rx"^Checking that user")
  ;          (lambda () (psql-create-user u)))
  ;        (void))]
  ;     [else
  ;      (displayln "TEST WARNING: cannot run psql-create-user success test, could not infer a valid DB user")])))

  ;; -- psql-create-db TODO

  ;; -- psql-create-tables TODO

  ;; -- psql-installed?, pass (machine-dependent)
  (check-true
    (check-print
      (list #rx"^Checking for `psql`")
      psql-installed?))

  ;; -- psql-installed?, fail TODO

  ;; -- psql-running, pass (machine-dependent)
  (check-true
    (check-print
      (list #rx"^Checking for psql server")
      psql-running?))

  ;; -- save-config TODO

  ;; -- start-server, pass
  (check-true
    (check-print
      (list
        #rx"^Checking"
        #rx"^Checking")
      start-server))
)
