;; -*- mode: scheme -*-

(define env (current-environment))

(define (repl)
  (display (eval (read) env))
  (repl))

(display "** Lisp Ready **")
(repl)
