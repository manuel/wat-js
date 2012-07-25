;; -*- mode: lisp -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def first (vau lst e (car (eval lst e))))
(def second (vau lst e (car (cdr (eval lst e)))))
(def third (vau lst e (car (cdr (cdr (eval lst e))))))
(def fourth (vau lst e (car (cdr (cdr (cdr (eval lst e)))))))

(def quote (vau y _ (car y)))
(def list (wrap (vau x _ x)))

(eval (list def (quote method-if) (vau form e (eval (first form) e)))
      (tagenv (tag-of #t)))
(eval (list def (quote method-if) (vau form e (eval (second form) e)))
      (tagenv (tag-of #f)))
(def if (vau form e ((eval (quote method-if) (tagenv (tag-of (eval (car form) e))))
		     (cdr form))))

(def assert (vau op e (if (eval op e) #void (fail op))))

(assert (eq #t #t))

