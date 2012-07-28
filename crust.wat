;; -*- mode: lisp -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def first (vau lst e (car (eval lst e))))
(def second (vau lst e (car (cdr (eval lst e)))))
(def third (vau lst e (car (cdr (cdr (eval lst e))))))
(def fourth (vau lst e (car (cdr (cdr (cdr (eval lst e)))))))

(def quote (vau y _ (car y)))
(def list (wrap (vau x _ x)))

(def assert (vau (expr) e (if (eval expr e) #void (fail expr))))

(def not (wrap (vau (expr) _ (if expr #f #t))))
