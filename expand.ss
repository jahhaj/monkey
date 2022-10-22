#!r6rs

(library
 (monkey expand)
 (export expand)
 (import
  (rnrs)
  (srfi parameters)
  )

 (define *env* (make-parameter 'todo))
 (define *phase* (make-parameter 0))

 (define (expand expr)
   ''todo)
 
 )
