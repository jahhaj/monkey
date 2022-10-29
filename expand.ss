#!r6rs

(library
 (monkey expand)
 (export expand-module expand-program expand-library expand)
 (import
  (rnrs base)
  (rnrs control)
  (rnrs io simple)
  (rnrs lists)
  (rnrs records syntactic)
  (srfi parameters)
  (monkey library)
  (monkey utility)
  )

 (define-record-type identifier (fields name colours envs disp))
 
 (define *env* (make-parameter 'todo))
 (define *phase* (make-parameter 0))

 (define expand-module
   (case-lambda
     ((path)
      (expand-module path #f))
     ((path lib-only?)
      (let ((forms (read-file path)))
        (if (null? forms)
            (error 'expand-module "no content in file" path)
            (cond
              ((and (list-1+? (car forms)) (eq? (caar forms) 'library))
               (if (list-1? forms)
                   (expand-library (car forms))
                   (error 'expand-module "excess content in library module" path)))
              ((and (list-1+? (car forms)) (eq? (caar forms) 'import) (not lib-only?))
               (expand-program forms))
              (else
               (error 'expand-module "expected program or library form in file" path))))))))

 (define (read-file path)
   (with-input-from-file path
     (lambda ()
       (let loop ((forms '()))
         (let ((form (read)))
           (if (eof-object? form)
               (reverse forms)
               (loop (cons form forms))))))))
 
 (define (expand-library form)
   (if (and (list-4+? form) (eq? (car form) 'library))
       (let-values (((name version) (parse-library-name (cadr form))))
         (parse-library-exports (caddr form))
         (write (parse-imports (cadddr form) 'expand-library))
         (newline)
         (expand-body 'library (cddddr form))
         'todo)
       (error 'expand-library "expected library form")))

 (define (expand-program forms)
   (if (and (list-1+? forms) (list-1+? (car forms)) (eq? (caar forms) 'import))
       (begin
         (write (parse-imports (car forms) 'expand-program))
         (newline)
         (expand-body 'program (cdr forms)))
       (error 'expand-program "expected import form")))
         
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
            (let ((version (car spec)))
              (if (and (list? version) (for-all exact-nonnegative-integer? version))
                  (values '() version)
                  (error 'expand-library "invalid library version" version))))
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
      (cons spec (source->syntax spec)))
     ((and (list-1+? spec) (eq? (car spec 'rename)) (for-all binding? (cdr spec)))
      (map (lambda (x y) (cons y (source->syntax x))) (cdr spec)))
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
   (unify < (map parse-import-level levels)))

 (define (parse-import-level level)
   (cond
     ((eq? level 'run)
      0)
     ((eq? level 'expand)
      1)
     ((and (list-2? level) (eq? (car level) 'meta) (exact-integer? (cadr level)))
      (cadr level))
     (else
      (error 'import "invalid import level" level))))

 (define (parse-import-set set)
   (define (binding? x) (and (list-2? x) (symbol? (car x)) (symbol? (cadr x))))
   (let loop ((set set) (filters '()))
     (cond
       ((and (list-2? set) (eq? (car set) 'library))
        (values (parse-library-ref (cadr set)) filters))
       ((and (list-2+? set) (eq? (car set) 'only) (for-all symbol? (cddr set)))
        (loop (cadr set) (cons (cons 'only (cddr set)) filters)))
       ((and (list-2+? set) (eq? (car set) 'except) (for-all symbol? (cddr set)))
        (loop (cadr set) (cons (cons 'except (cddr set)) filters)))
       ((and (list-3? set) (eq? (car set) 'prefix) (symbol? (caddr set)))
        (loop (cadr set) (cons (list 'prefix (caddr set)) filters)))
       ((and (list-2+? set) (eq? (car set) 'rename) (for-all binding? (cddr set)))
        (loop (cadr set) (cons (cons 'rename (cddr set)) filters)))
       ((and (pair? set) (memq (car set) '(library only except prefix rename for)))
        (error 'import "invalid import set" set))
       (else
        (values (parse-library-ref set) filters)))))

 (define (parse-library-ref ref)
   (define (wrong) (error 'import "invalid library reference" ref))
   (if (and (list-1+? ref) (symbol? (car ref)))
       (let-values (((name version-ok?)
                     (let loop ((ref ref))
                       (cond
                         ((null? ref)
                          (values '() (lambda (v) #t)))
                         ((symbol? (car ref))
                          (let-values (((name version-ok?) (loop (cdr ref))))
                            (values (cons (car ref) name) version-ok?)))
                         ((null? (cdr ref))
                          (values '() (parse-library-version-ref (car ref))))
                         (else
                          (wrong))))))
         (let ((lib (find-library name)))
           (if lib
               (if (version-ok? (library-version lib))
                   lib
                   (error 'import "incorrect library version"))
               'todo)))
       (wrong)))

 (define (parse-library-version-ref ref)
   'todo)

 (define (expand-body type forms)
   'todo)
 
 (define (expand expr)
   ''todo)

 (define source->syntax
   (let ((toplevel-id (make-identifier 'toplevel '() '() 0)))
     (lambda (x)
       (datum->syntax toplevel-id x))))

 (define (datum->syntax tid x)
   (let ((colours (identifier-colours tid))
         (envs (identifier-envs tid))
         (disp (identifier-disp tid)))
     (sexpr-map (lambda (leaf)
                  (if (symbol? leaf)
                      (make-identifier leaf colours envs disp)
                      leaf))
                x)))

 (define (syntax->datum x)
   (sexpr-map (lambda (leaf)
                (cond
                  ((identifier? leaf)
                   (identifier-name leaf))
                  ((symbol? leaf)
                   (assertion-violation 'syntax->datum "symbol in syntax object"))
                  (else
                   leaf)))
              x))
 
 (define (sexpr-map proc x)
   (let loop ((x x))
     (cond
       ((null? x)
        '())
       ((pair? x)
        (let ((new-car (loop (car x)))
              (new-cdr (loop (cdr x))))
          (if (and (eq? new-car (car x)) (eq? new-cdr (cdr x)))
              x
              (cons new-car new-cdr))))
       ((vector? x)
        (let* ((old (vector->list x))
               (new (loop old)))
          (if (eq? new old)
              x
              (list->vector new))))
       (else
        (proc x)))))

 (define gen-label
   (case-lambda
     (() (gensym "lbl:"))
     ((x) (gensym))))
 
 ;; create a mini rnrs base library
 (let* ((prims '(cons car cdr))
        (exports (map (lambda (p) (cons p p)) prims))
        (bindings (map (lambda (p) (make-binding 'variable #f '(0))) prims))
        (lib (make-library '(rnrs base) '(6) exports bindings)))
   (install-library! lib))
 
   
 )
