;; -*- mode: scheme -*-

(define env (current-environment))

(define (repl)
  (define (loop)
    (display (eval (read) env))
    (loop))
  (push-prompt *top-level*
    (loop)))

(display "Welcome to Wat -10.3")
(repl)
