;; -*- mode: scheme -*-

(define env (current-environment))

(define *top-level* (make-prompt))

(define (repl)
  (display (->string (eval (read) env)))
  (repl))

(display "** Lisp Ready **")
(repl)
