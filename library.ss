#!r6rs

(library
 (monkey library)
 (export make-binding binding-type
         find-library install-library!
         make-library library-version)
 (import
  (rnrs)
  (monkey utility)
  )

 (define-record-type binding (fields type value levels))
 
 (define-record-type library (fields id name version exports bindings)
   (protocol (lambda (p)
               (lambda (name version exports bindings)
                 (p (gensym "lib:") name version exports bindings)))))

 (define *all-libraries* (make-hashtable equal-hash equal?)) ;; TODO parameterize?
 
 (define (find-library name)
   (hashtable-ref *all-libraries* name #f))

 (define (install-library! lib)
   (hashtable-update! *all-libraries*
                      (library-name lib)
                      (lambda (old-lib)
                        (assert (eq? lib old-lib))
                        lib)
                      lib))
 
 )
