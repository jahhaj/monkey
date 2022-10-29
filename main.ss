#!r6rs

(import
 (rnrs base)
 (monkey eval)
 (monkey expand)
 )

(expand-program '((import (rnrs base)) (cons 1 2)))

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
