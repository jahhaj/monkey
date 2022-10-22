#!r6rs

(library
 (monkey expand)
 (export expand-library expand)
 (import
  (rnrs)
  (srfi parameters)
  )

 (define *env* (make-parameter 'todo))
 (define *phase* (make-parameter 0))

 (define (expand-library form)
   (if (and (list-4+? form) (eq? (car form) 'library))
       (let-values (((name version) (parse-library-name (cadr form))))
         (parse-library-exports (caddr form))
         (parse-imports (cadddr form) 'expand-library)
         (expand-body 'library (cddddr form))
         'todo)
       (error 'expand-library "expected library form")))

 (define (parse-library-name spec)
   (define (wrong) (error 'expand-library "invalid library name" spec))
   (if (and (list-1+? spec) (symbol? (car spec)))
       (let loop ((spec spec))
         (cond
           ((null? spec)
            (values '() '()))
           ((symbol? (car spec))
            (let-values (((name version) (loop (cdr spec))))
              (values (cons (car spec) name) version)))
           ((list-1? spec)
            (if (and (list? spec) (for-all integer? spec) (for-all exact? spec) (not (exists negative? spec)))
                (values '() (car spec))
                (error 'expand-library "invalid library version" spec)))
           (else
            (wrong))))
       (wrong)))
 
 (define (parse-library-exports spec)
   (if (and (list-1+? spec) (eq? (car spec) 'export))
       (apply append (map parse-export (cdr spec)))
       (error 'expand-library "expected library exports")))

 (define (parse-export spec)
   (define (binding? x) (and (list-2? x) (symbol? (car x)) (symbol? (cadr x))))
   (cond
     ((symbol? spec)
      (cons spec (source->stx spec)))
     ((and (list-1+? spec) (eq? (car spec 'rename)) (for-all binding? (cdr spec)))
      (map (lambda (x y) (cons y (source->stx x))) (cdr spec)))
     (else
      (error 'expand-library "invalid export" spec))))
 
 (define (parse-imports spec who)
   (if (and (list-1+? spec) (eq? (car spec) 'import))
       'todo
       (error who "expected library imports")))

 (define (expand-body type forms)
   'todo)
 
 (define (expand expr)
   ''todo)

 (define (source->stx x)
   'todo)
 
 ;;;
 ;;; utilities
 ;;;

 (define (list-1? x) (and (pair? x) (null? (cdr x))))
 (define (list-2? x) (and (pair? x) (list-1? (cdr x))))
 (define (list-3? x) (and (pair? x) (list-2? (cdr x))))
 (define (list-4? x) (and (pair? x) (list-3? (cdr x))))

 (define (list-1+? x) (and (pair? x) (list? (cdr x))))
 (define (list-2+? x) (and (pair? x) (list-1+? (cdr x))))
 (define (list-3+? x) (and (pair? x) (list-2+? (cdr x))))
 (define (list-4+? x) (and (pair? x) (list-3+? (cdr x))))
 
 )
