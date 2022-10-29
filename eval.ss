#!r6rs

(library
 (monkey eval)
 (export core-eval)
 (import
  (rnrs base)
  (rnrs eval))
 
 (define core-eval
   (let ((env (environment '(rnrs) '(rnrs eval) '(rnrs mutable-pairs) '(rnrs mutable-strings) '(rnrs r5rs))))
     (lambda (expr) (eval expr env))))
 
 )
