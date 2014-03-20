;;;;; Wat Test Suite

;;;; Utilities

(define (cat . objects)
  (#join (list-to-array objects) ""))

(define (log . objects)
  (apply #log (list* @console objects)))

(define assert-true
  (vau (expr) e
    (unless (=== true (eval expr e))
      (error (+ "Should be true: " expr)))))

(define assert-false
  (vau (expr) e
    (unless (=== false (eval expr e))
      (error (+ "Should be false: " expr)))))

(define assert-equal
  (vau (expr1 expr2) e
    (unless (=== (eval expr1 e) (eval expr2 e))
      (error (cat "Should be equal: " expr1 " and " expr2)))))

(define assert-throws
  (vau (expr) e
    (label return
      (catch (eval expr e)
        (lambda (exc) (return)))
      (error (+ "Should throw: " expr)))))

(define (--print-stacktrace-and-throw err)
  (define (print-frame k)
    (#log @console (#toString (.dbg k)) (.e k))
    (if (.next k)
      (print-frame (.next k))
      null))
  (take-subcont --root-prompt k
    (print-frame k)
    (push-prompt --root-prompt
      (push-subcont k
        (throw err)))))
