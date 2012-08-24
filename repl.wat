;; -*- mode: scheme -*-

(define env (current-environment))

(define *top-level* (make-prompt))

(define (repl)
  (push-prompt *top-level* (display (->string (eval (read) env))) (repl)))

(display "** Lisp Ready **")
(repl)
