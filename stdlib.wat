(define provide
  (vau (symbolz &rest body) env
    (eval (list def symbolz
            (list let ()
              (list* begin body)
              (list* list symbolz)))
          env)))

(define module
  (vau (exports &rest body) e
    (let ((env (make-environment e)))
      (eval (list* provide exports body) env)
      env)))

(define define-module
  (vau (name exports &rest body) e
    (eval (list define name (list* module exports body)) e)))

(define import
  (vau (module imports) e
    (let* ((m (eval module e))
           (values (map-list (lambda (import) (eval import m)) imports)))
      (eval (list def imports (list* list values))
            e))))

