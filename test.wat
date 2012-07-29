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

  ;; CALL/CC

  (scope ()
    (assert (eq #t (call/cc (lambda (k) (k #t) #f))))
    (assert (eq #f (call/cc (lambda #ign #f)))))

  ;; VAU

  (scope ()
    (def env (current-environment))
    (eq #t ((vau x #ign x) #t))
    (eq #t ((vau (x . #ign) #ign x) (list #t)))
    (eq env ((vau #ign e e))))

  ;; EVAL

  (scope ()
    (def env (current-environment))
    (eval (list def (quote x) #t) env)
    (assert (eq x #t))
    
    (assert (eq (eval #t env) #t)))

  ;; WRAP

  (scope ()
    (assert (eq #t ((wrap (vau (x) #ign x)) (not #f)))))

  ;; UNWRAP

  (scope ()
    (assert (eq list (unwrap (wrap list)))))

  ;; EQ

  (scope ()
    (assert (eq #t #t))
    (assert (not (eq #t #f)))
    (assert (not (eq (list 1) (list 1)))))

  ;; CONS

  (scope ()
    (assert (eq #t (car (cons #t #f))))
    (assert (eq #f (cdr (cons #t #f)))))

  ;; MAKE-ENVIRONMENT

  (scope ()
    (def e1 (make-environment))
    (eval (list def (quote x) #t) e1)
    (eval (list def (quote y) #t) e1)
    (assert (eq #t (eval (quote x) e1)))
    (assert (eq #t (eval (quote y) e1)))

    (def e2 (make-environment e1))
    (assert (eq #t (eval (quote x) e2)))
    (assert (eq #t (eval (quote y) e2)))
    (eval (list def (quote y) #f) e2)
    (assert (eq #f (eval (quote y) e2)))
    (assert (eq #t (eval (quote y) e1))))

  ;; MAKE-TYPE
 
  (scope ()
    (def type (make-type))
    (assert (eq (type-of type) (type-of (type-of #t)))))

  ;; TYPE-ENVIRONMENT

  (scope ()
    (def type (make-type))
    (def tenv (type-environment type))
    (assert (eq (type-of tenv) (type-of (current-environment)))))

  ;; TYPE-OF

  (scope ()
    (assert (not (eq (type-of () #void))))
    (assert (eq (type-of 0) (type-of 1))))

  ;; TAG, UNTAG

  (scope ()
    (def type (make-type))
    (def tagged (tag type #t))
    (assert (eq (type-of tagged) type))
    (assert (eq (untag tagged) #t)))

)
