#!r6rs

(import
 (rnrs)
 (monkey eval)
 (monkey expand)
 )

(expand-module "lib/rnrs/base.ss")

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
