
(define provide
  (vau (symbols . body) env
    (eval (list def symbols
            (list let ()
              (list* begin body)
              (list* list symbols)))
          env)))

(define module
  (vau (exports . body) e
    (let ((env (make-environment e)))
      (eval (list* provide exports body) env)
      env)))

(define define-module
  (vau (name exports . body) e
    (eval (list define name (list* module exports body)) e)))

(define import
  (vau (module imports) e
    (let* ((m (eval module e))
           (values (map-list (lambda (import) (eval import m)) imports)))
      (eval (list def imports (list* list values))
            e))))

