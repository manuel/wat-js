;; -*- mode: scheme -*-

(define env (current-environment))

(define *top-level* (make-prompt))

(define (repl)
  (until (blocking? (push-prompt *top-level* (display (->string (eval (read) env)))))))

(display "** Lisp Ready **")
(repl)
