#|
    Module for "rendering" page data -- returns data structures and appopriate lookup procedure
    to be used interpolating html templates with mustache.
|#
(define-library
  (scmindex renderer)
  (import (scheme base)
          (scheme char)
          (scheme read)
          (scheme write)
          (scheme cxr)
          (arvyy mustache)
          (arvyy kawa-spark)
          (scmindex domain)
          (scmindex util)
          (only (srfi 1) iota filter find any partition)
          (srfi 95))
  
  (export

    ;; render page functions -- each returning 2 values: template name and template data
    render-home-page
    render-search-page
    render-settings-page

    ;; list of cookies settings uses
    settings-cookies

    ;; mustache lookup function, so that mustache knows how to render here defined records
    data-lookup
    
    ;; return user settings as saved in the cookies
    user-setting/page-size
    user-setting/param-filter-loose

    ;; exported for testing only
    render-index-entry)

  (begin
    (include "renderer/common.scm")
    (include "renderer/home.scm")
    (include "renderer/search.scm")
    (include "renderer/settings.scm")
    ;; compose all lookup procedures into one
    (define data-lookup
      (compose-lookups
        sexpr-el-lookup
        navigation-lookup
        nav-item-lookup
        nav-item-extra-lookup
        pager-button-lookup
        facet-lookup
        facet-option-lookup
        search-result-mustache-lookup
        result-item-lookup
        page-head-lookup
        setting-lookup
        setting-option-lookup
        page-lookup
        syntax-rule-lookup
        result-item-extra-lookup
        facet-extra-lookup
        spec-value-fieldblock-lookup
        spec-value-fieldentry-lookup
        filterset-group-lookup
        filterset-entry-lookup
        home-download-lookup
        home-body-lookup
        home-body-extra-lookup))))
