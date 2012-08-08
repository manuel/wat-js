;; -*- mode: scheme -*-

(define env (current-environment))

(define (repl)
  (define (loop)
    (push-prompt *top-level*
      (display (eval (read) env))
      (loop)))
  (loop))

(display "Welcome to Wat -10.3")
(repl)
