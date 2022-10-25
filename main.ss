#!r6rs

(import
 (rnrs)
 (monkey eval)
 (monkey expand)
 )

(expand-library '(library (test) (export) (import (prefix (only (rnrs) cons) std))))

#|
(let loop ()
  (display "$ ")
  (let ((expr (read)))
    (unless (eof-object? expr)
      (let ((expr (expand expr)))
        (call-with-values
         (lambda () (core-eval expr))
         (lambda results
           (for-each (lambda (res)
                       (write res)
                       (newline))
                     results))))
      (loop))))
|#
