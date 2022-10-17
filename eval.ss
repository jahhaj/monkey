#!r6rs

(library
 (monkey eval)
 (export core-eval symbol-value set-symbol-value! undef undef?)
 (import
  (rnrs)
  (rnrs mutable-pairs))

 (define-record-type (undef-rtd make-undef undef?))

 (define *undef* (make-undef))
 (define *global-env* (make-eq-hashtable))
 
 (define (core-eval expr)
   (eval expr '()))

 (define (eval expr env)
   (define (E x) (eval x env))
   (cond
     ((symbol? expr)
      (lookup expr env))
     ((pair? expr)
      (case (car expr)
        ((lambda)
         (let ((params (cadr expr))
               (body (caddr expr)))
           (lambda args
             (eval body
                   (let loop ((params params) (args args) (env env))
                     (cond
                       ((null? params)
                        (unless (null? args)
                          (error 'core-eval "arity error"))
                        env)
                       ((symbol? params)
                        (cons (cons params args) env))
                       (else
                        (unless (pair? args)
                          (error 'core-eval "arity error"))
                        (loop (cdr params) (cdr args) (cons (cons (car params) (car args)) env)))))))))
        ((if)
         (if (E (cadr expr))
             (E (caddr expr))
             (E (cadddr expr))))
        ((quote)
         (cadr expr))
        ((begin)
         (let loop ((first (cadr expr)) (rest (cddr expr)))
           (if (null? rest)
               (E first)
               (begin
                 (E first)
                 (loop (car rest) (cdr rest))))))
        ((set!)
         (update! (cadr expr) (E (caddr expr)) env))
        (else
         (let ((proc (E (car expr))))
           (apply proc (map E (cdr expr)))))))
     (else
      expr)))

 (define (lookup var env)
   (cond
     ((assq var env) => (lambda (p)
                          (let ((val (cdr p)))
                            (when (undef? val)
                              (error 'core-eval "undefined variable" var))
                            val)))
     (else (symbol-value var))))

 (define (update! var val env)
   (cond
     ((assq var env) => (lambda (p)
                          (set-cdr! p val)))
     (else (set-symbol-value! var val))))
 
 (define (symbol-value sym)
   (hashtable-ref *global-env* sym *undef*))

 (define (set-symbol-value! sym val)
   (hashtable-set! *global-env* sym val))

 (define (undef) *undef*)
 
 )
