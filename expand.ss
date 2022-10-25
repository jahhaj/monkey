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
         (write (parse-imports (cadddr form) 'expand-library))
         (newline)
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
       (map parse-import-spec (cdr spec))
       (error who "expected library imports")))

 (define (parse-import-spec spec)
   (if (and (list-2+? spec) (eq? (car spec) 'for))
       (let-values (((libs filters) (parse-import-set (cadr spec))))
         (list libs filters (parse-import-levels (cddr spec))))
       (let-values (((libs filters) (parse-import-set spec)))
         (list libs filters '(0)))))

 (define (parse-import-levels levels)
   (unify < = (map parse-import-level levels)))

 (define (parse-import-level level)
   (cond
     ((eq? level 'run)
      0)
     ((eq? level 'expand)
      1)
     ((and (list-2? level)
           (eq? (car level) 'meta)
           (integer? (cadr level))
           (exact? (cadr level)))
      (cadr level))
     (else
      (error 'import "invalid import level" level))))

 (define (parse-import-set set)
   (define (binding? x) (and (list-2? x) (symbol? (car x)) (symbol? (cadr x))))
   (let loop ((set set) (filters '()))
     (cond
       ((and (list-2? set) (eq? (car set) 'library))
        (values (parse-library-ref (cadr set)) filters))
       ((and (list-1+? set) (eq? (car set) 'primitives) (for-all symbol? (cdr set)))
        (values 'todo filters))
       ((and (list-2+? set) (eq? (car set) 'only) (for-all symbol? (cddr set)))
        (loop (cadr set) (cons (cons 'only (cddr set)) filters)))
       ((and (list-2+? set) (eq? (car set) 'except) (for-all symbol? (cddr set)))
        (loop (cadr set) (cons (cons 'except (cddr set)) filters)))
       ((and (list-3? set) (eq? (car set) 'prefix) (symbol? (caddr set)))
        (loop (cadr set) (cons (list 'prefix (caddr set)) filters)))
       ((and (list-2+? set) (eq? (car set) 'rename) (for-all binding? (cddr set)))
        (loop (cadr set) (cons (cons 'rename (cddr set)) filters)))
       ((and (pair? set) (memq (car set) '(library primitives only except prefix rename for)))
        (error 'import "invalid import set" set))
       (else
        (values (parse-library-ref set) filters)))))

 (define (parse-library-ref ref)
   'todo)
 
 (define (expand-body type forms)
   'todo)
 
 (define (expand expr)
   ''todo)

 (define (source->stx x)
   'todo)
 
 ;;;
 ;;; utilities
 ;;;

 ;; sort a list and remove deuplicates
 (define (unify lt eq items)
   (let loop ((items (list-sort lt items)))
     (cond
       ((not (list-2+? items))
        items)
       ((eq (car items) (cadr items))
        (loop (cdr items)))
       (else
        (let ((rest (loop (cdr items))))
          (if (eq? rest (cdr items))
              items
              (cons (car items) rest)))))))
 
 (define (list-1? x) (and (pair? x) (null? (cdr x))))
 (define (list-2? x) (and (pair? x) (list-1? (cdr x))))
 (define (list-3? x) (and (pair? x) (list-2? (cdr x))))
 (define (list-4? x) (and (pair? x) (list-3? (cdr x))))

 (define (list-1+? x) (and (pair? x) (list? (cdr x))))
 (define (list-2+? x) (and (pair? x) (list-1+? (cdr x))))
 (define (list-3+? x) (and (pair? x) (list-2+? (cdr x))))
 (define (list-4+? x) (and (pair? x) (list-3+? (cdr x))))
 
 )
