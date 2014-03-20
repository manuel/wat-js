;; -*- mode: scheme -*-

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

(assert-throws (lambda))
;(assert-throws (lambda 12 12))
;(assert-throws (lambda "foo" "bar"))
(assert-throws (def))
(assert-throws (def 12))
(assert-throws (def 12 12))

(assert-equal null (begin))
(assert-equal 1 (begin 1))
(assert-equal 3 (begin 1 2))

(define-macro (provide symbols . body)
  (list define symbols
    (list let ()
      (list* begin body)
      (list* list symbols))))

