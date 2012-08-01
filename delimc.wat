;; -*- mode: scheme -*-

(provide (push-prompt push-sub-cont)
  (define-syntax push-prompt
    (vau (p . es) env
      (push-prompt* (eval p env)
        (lambda () (eval (list* begin es) env)))))
  (define-syntax push-sub-cont
    (vau (k . es) env
      (push-sub-cont* (eval k env)
        (lambda () (eval (list* begin es) env)))))
)
