;; -*- mode: scheme -*-
;; This file contains JS-specific Wat code that implements lowlevel functionality.

(define (stacktrace)
  (define (continuation->list k)
    (if (eq? js-null k)
        ()
        (cons (js-prop k "dbginfo") (continuation->list (js-prop k "next")))))
  (take-subcont *top-level* k
    (push-prompt *top-level*
      (push-subcont k
        (continuation->list k)))))

(define (display-stacktrace tr)
  (for-each (lambda (frame)
              (unless (void? frame)
                (display (->string frame))))
            tr))
