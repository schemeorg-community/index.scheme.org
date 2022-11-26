(define-mustache-record-type <home-download>
  home-download-lookup
  (make-home-download name url checksum)
  home-download?
  (name home-download-name)
  (url home-download-url)
  (checksum home-download-checksum))

(define-mustache-record-type <home-body>
  home-body-lookup
  (make-home-body filterset-groups downloads)
  home-body?
  (filterset-groups home-body-filterset-groups)
  (downloads home-body-downloads))

(define-mustache-record-type <filterset-group>
  filterset-group-lookup
  (make-filterset-group filtersets)
  filterset-group?
  (filtersets filterset-group-filtersets))

(define-mustache-record-type <filterset-entry>
  filterset-entry-lookup
  (make-filterset-entry code name)
  filterset-entry?
  (code filterset-entry-code)
  (name filterset-entry-name))

(define (home-body-extra-lookup obj name found not-found)
  (cond
   ((not (home-body? obj)) (not-found))
   ((equal? "has-downloads?" name)
    (let ((downloads (home-body-downloads obj)))
      (found (and downloads
                  (not (null? downloads))))))
   (else (not-found))))

(define (make-filterset-groups filtersets-alist)
  (define filtersets (map (lambda (e)
                            (make-filterset-entry (car e) (cdr e)))
                          filtersets-alist))
  (define spec '("r5rs" "r6rs" "r7rs"))
  (define (spec? filterset)
    (define code (filterset-entry-code filterset))
    (any (lambda (spec-code)
           (and (>= (string-length code) (string-length spec-code))
                (string=? spec-code (substring code 0 (string-length spec-code)))))
         spec))
  (define-values (spec-filtersets impl-filtersets)
    (partition spec? filtersets))
  (list (make-filterset-group spec-filtersets)
        (make-filterset-group impl-filtersets)))

(define (make-downloads-section downloads)
  (cond
   ((not downloads) #f)
   (else (map
          (lambda (e)
            (make-home-download (cdr (assoc 'name e))
                                (cdr (assoc 'url e))
                                (cdr (assoc 'checksum e))))
          downloads))))

(define (render-home-page settings downloads-config filtersets)
  (values
   "index"
   (make-page
    (make-page-head #f)
    (make-navigation (make-mustache-nav-data 'index filtersets (deploy-setting/enable-user-settings settings)))
    (make-home-body
     (make-filterset-groups filtersets)
     (make-downloads-section downloads-config)))))
