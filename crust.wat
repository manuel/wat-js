;; -*- mode: scheme -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def first (vau lst e (car (eval lst e))))
(def second (vau lst e (car (cdr (eval lst e)))))
(def third (vau lst e (car (cdr (cdr (eval lst e))))))
(def fourth (vau lst e (car (cdr (cdr (cdr (eval lst e)))))))

(def quote (vau form _ (second form)))

(def assert (vau form e (if (eval (second form) e) #void (fail (second form)))))

;; if
(eval (cons def (cons (quote method-if) (cons (vau form e (eval (first form) e)) ())))
      (tagenv (tag-of #t)))
(eval (cons def (cons (quote method-if) (cons (vau form e (eval (second form) e)) ())))
      (tagenv (tag-of #f)))
(def if (vau form e ((eval (quote method-if) (tagenv (tag-of (eval (second form) e))))
		     (cdr (cdr form)))))

(assert (eq (if T T F) T))
(assert (eq (if F T F) F))

