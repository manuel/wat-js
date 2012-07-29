(scope ()

  ;;;;; Test Core Language

  ;; DEF

  (scope ()
    (def (x y) (list #t #f))
    (assert (eq x #t))
    (assert (eq y #f))
    
    (assert (eq (def #ign #t) #t)))

  ;; IF

  (scope ()
    (assert (eq #t (if #t #t #f)))
    (assert (eq #f (if #f #t #f))))

  (assert (eq #t #t))
  (assert (not (eq #t #f)))
  (assert (eq #t (call/cc (lambda (k) (k #t) #f))))
  (assert (eq #f (call/cc (lambda #ign #f))))

)
