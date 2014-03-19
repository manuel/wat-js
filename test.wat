(define (cat . objects)
  (#join (list-to-array objects) ""))

(define (log . objects)
  (apply #log (list* @console objects)))

(def assert-true
  (vau (expr) e
    (unless (=== true (eval expr e))
      (throw (+ "Should be true: " expr)))))

(def assert-equal
  (vau (expr1 expr2) e
    (unless (=== (eval expr1 e) (eval expr2 e))
      (throw (cat "Should be equal: " expr1 " and " expr2)))))

(def assert-false
  (vau (expr) e
    (unless (=== false (eval expr e))
      (throw (+ "Should be false: " expr)))))

(def assert-throws
  (vau (expr) e
    (label return
      (catch (eval expr e)
        (lambda (exc) (return)))
      (throw (+ "Should throw: " expr)))))

;; Test the testers

(assert-throws (assert-true false))
(assert-throws (assert-false true))
(assert-throws (assert-equal 1 2))
(assert-throws (assert-throws "baz"))

(assert-true (=== 2 2))
(assert-false (!== 2 2))

(assert-throws (throw "foo"))


