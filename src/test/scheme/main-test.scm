(import (scheme base)
        (scheme process-context)
        (srfi 64)
        (kawa-spark-test main-test)
        (mustache-test main-test)
        (scmindex-test main-test))

(test-begin "Kawa web test")

;; on test end exit with non-zero status if there were failures
(let* ((runner (test-runner-current))
       (callback (test-runner-on-final runner)))
  (test-runner-on-final!
    runner
    (lambda (r)
      (callback r)
      (exit (= 0 (test-runner-fail-count r))))))

(do-kawa-spark-test)
(do-mustache-test)
(do-scmindex-test)

(test-end)
