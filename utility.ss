#!r6rs

(library
 (monkey utility)
 (export gensym
         unify
         list-1? list-2? list-3? list-4? list-1+? list-2+? list-3+? list-4+?
         exact-integer? exact-nonnegative-integer?)
 (import
  (rnrs arithmetic bitwise)
  (rnrs base)
  (rnrs control)
  (rnrs sorting)
  (only (racket base) random)
  )

 (define gensym
   (case-lambda
     (()
      (gensym "g"))
     ((prefix)

      (define (make-uuid)
        (let loop ((uuid 0) (i 0))
          (if (= i 8)
              (bitwise-copy-bit-field (bitwise-copy-bit-field uuid 62 64 2) 76 80 4)
              (loop (bitwise-ior (bitwise-arithmetic-shift-left uuid 16) (random #x10000)) (+ i 1)))))
      
      (define base64-table (vector #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                            #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                            #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))

      (define (base64-encode n)
        (let loop ((i 0) (n n) (res '()))
          (if (= i 22)
              (apply string res)
              (loop (+ i 1) (bitwise-arithmetic-shift-right n 6) (cons (vector-ref base64-table (bitwise-bit-field n 0 6)) res)))))
      
      (if (symbol? prefix)
          (gensym (symbol->string prefix))
          (string->symbol (string-append prefix (base64-encode (make-uuid))))))))
 
 ;; sort a list and remove deuplicates
 (define (unify < items)
   (define (= x y) (not (or (< x y) (< y x))))
   (let loop ((items (list-sort < items)))
     (cond
       ((not (list-2+? items))
        items)
       ((= (car items) (cadr items))
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

 (define (exact-integer? x) (and (integer? x) (exact? x)))
 (define (exact-nonnegative-integer? x) (and (exact-integer? x) (not (negative? x))))

 )
