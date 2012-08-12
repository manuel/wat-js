;; -*- mode: scheme -*-

(define env (current-environment))

(define (repl)
  (push-prompt *top-level*
    (display (->string (eval (read) env))))
  (repl))

(display "** Lisp Ready **")
(repl)
