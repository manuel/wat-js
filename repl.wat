;; -*- mode: scheme -*-

(define env (current-environment))

(define *top-level* (make-prompt))

(define (repl)
  (define (loop)
    (display (->string (eval (read) env)))
    (loop))
  (push-prompt *top-level*
    (loop)))

(display "** Lisp Ready **")
(repl)
