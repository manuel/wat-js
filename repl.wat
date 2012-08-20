;; -*- mode: scheme -*-

(define env (current-environment))

(define (repl)
  (loop (display (->string (eval (read) env)))))

(display "** Lisp Ready **")
(repl)
