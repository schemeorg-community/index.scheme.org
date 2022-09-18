(define-library
  (scmindex filterset-store)
  (import (scheme base)
          (scmindex domain)
          (arvyy dbutils))
  (export make-sqlite-filterset-store)
  (begin

    (define (init-db qr)
      (call-in-transaction 
        qr 
        #t 
        (lambda ()
          (update qr "drop table if exists filterset")
          (update qr 
                  "create table filterset(
                    code text,
                    source text,
                    target text)")
          (update qr "create index filterset_name_source on filterset (code, source)")
          (update qr "create index filterset_name_target on filterset (code, target)")
          (update qr "drop table if exists filterset_names")
          (update qr "create table filterset_names(code text, name text)")
          (commit))))

    (define (save-filterset-entries* qr entries)
      (call-in-transaction qr #t (lambda ()
                                   (update qr "delete from filterset")
                                   (for-each
                                     (lambda (e)
                                       (update qr "insert into filterset(code, source, target) values(?, ?, ?)"
                                               (filter-entry-filter e)
                                               (filter-entry-source e)
                                               (filter-entry-target e)))
                                     entries)
                                   (commit))))

    (define (save-filterset-names* qr names)
      (call-in-transaction qr #t (lambda ()
                                   (update qr "delete from filterset_names")
                                   (for-each
                                     (lambda (e)
                                       (update qr "insert into filterset_names(code, name) values(?, ?)"
                                               (car e)
                                               (cdr e)))
                                     names)
                                   (commit))))

    (define (code-list* qr)
      (define result (query qr "select distinct code from filterset_names order by name asc" (handler/list-of (row-handler/vector))))
      (map
        (lambda (r)
          (vector-ref r 0))
        result))

    (define (get-name* qr code)
      (define result (query qr "select name from filterset_names where code = ?" (handler/list-of (row-handler/vector)) code))
      (if (null? result)
          #f
          (vector-ref (car result) 0)))

    (define (source-list* qr code)
      (define result (query qr "select source from filterset where code = ?" (handler/list-of (row-handler/vector)) code))
      (map
        (lambda (r)
          (vector-ref r 0))
        result))

    (define (get-target* qr code source)
      (define result (query qr "select target from filterset where code = ? and source = ?" (handler/list-of (row-handler/vector)) code source))
      (if (null? result)
          #f
          (vector-ref (car result) 0)))

    (define (get-source* qr code target)
      (define result (query qr "select source from filterset where code = ? and target = ?" (handler/list-of (row-handler/vector)) code target))
      (if (null? result)
          #f
          (vector-ref (car result) 0)))

    (define (make-sqlite-filterset-store location)
      (define qr (make-query-runner "org.sqlite.JDBC" (string-append "jdbc:sqlite:" location) "" ""))
      (init-db qr)
      (make-filterset-store
        ((save-filterset-entries entries)
         (save-filterset-entries* qr entries))
        ((save-filterset-names entries)
         (save-filterset-names* qr entries))
        ((get-name code)
         (get-name* qr code))
        ((code-list)
         (code-list* qr))
        ((source-list code)
         (source-list* qr code))
        ((get-target code source)
         (get-target* qr code source))
        ((get-source code target)
         (get-source* qr code target))))

))
