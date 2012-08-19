;; -*- mode: scheme -*-

(define env (current-environment))

(define (repl)
  (loop (display (eval (read) env))))

(display "** Lisp Ready **")
(repl)
