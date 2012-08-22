;; -*- mode: scheme -*-

(define env (current-environment))

(define *top-level* (make-prompt))

(define (repl)
  (push-prompt *top-level* (display (->string (eval (read) env)))))

;(display (let ((p (make-prompt))) (+ 2 (push-prompt p (if (take-sub-cont p k (+ (push-sub-cont k #f) (push-sub-cont k #t))) 3 4)))))
(display "** Lisp Ready **")
(repl)
