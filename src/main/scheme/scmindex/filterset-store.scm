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
          (update qr 
                  "create table if not exists filterset(
                    name text,
                    source text,
                    target text)")
          (update qr "create index if not exists filterset_name_source on filterset (name, source)")
          (update qr "create index if not exists filterset_name_target on filterset (name, target)"))))

    (define (save-filterset-entries* qr entries)
      (call-in-transaction qr #t (lambda ()
                                   (update qr "delete from filterset")
                                   (for-each
                                     (lambda (e)
                                       (update qr "insert into filterset(name, source, target) values(?, ?, ?)"
                                               (filter-entry-filter e)
                                               (filter-entry-source e)
                                               (filter-entry-target e)))
                                     entries)
                                   (commit))))

    (define (name-list* qr)
      (define result (query qr "select distinct name from filterset" (handler/list-of (row-handler/vector))))
      (map
        (lambda (r)
          (vector-ref r 0))
        result))

    (define (source-list* qr filtername)
      (define result (query qr "select source from filterset where name = ?" (handler/list-of (row-handler/vector)) filtername))
      (map
        (lambda (r)
          (vector-ref r 0))
        result))

    (define (get-target* qr filtername source)
      (query qr "select target from filterset where name = ? and source = ?" (handler/scalar) filtername source))

    (define (get-source* qr filtername target)
      (query qr "select source from filterset where name = ? and target = ?" (handler/scalar) filtername target))

    (define (make-sqlite-filterset-store location)
      (define qr (make-query-runner "org.sqlite.JDBC" (string-append "jdbc:sqlite:" location) "" ""))
      (init-db qr)
      (make-filterset-store
        ((save-filterset-entries entries)
         (save-filterset-entries* qr entries))
        ((name-list)
         (name-list* qr))
        ((source-list filtername)
         (source-list* qr filtername))
        ((get-target filtername source)
         (get-target* qr filtername source))
        ((get-source filtername target)
         (get-source* qr filtername target))))

))
