;; -*- mode: scheme -*-

(define env (current-environment))

(define *top-level* (make-prompt))

(define (repl)
  (push-prompt *top-level*
    (loop (display (->string (eval (read) env))))))

(display "** Lisp Ready **")
(repl)
