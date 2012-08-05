;; -*- mode: scheme -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def null? (wrap (vau (val) #ign (eq? () val))))

(def pair? (wrap (vau (val) #ign (eq? (type-of val) (type-of (cons #void #void))))))

(def symbol? (wrap (vau (val) #ign (eq? (type-of val) (type-of 'foo)))))

(def procedure? (wrap (vau (val) #ign (eq? (type-of val) (type-of (wrap (vau #ign #ign #void)))))))

;; (def begin
;;   ((wrap (vau (seq2) #ign
;; 	   (seq2
;;              (def aux
;;                (vau (head . tail) env
;; 		 (if (null? tail)
;; 		     (eval head env)
;; 		     (seq2 (eval head env) (eval (cons aux tail) env)))))
;;              (vau body env
;;                (if (null? body)
;;                    #void
;;                    (eval (cons aux body) env))))))
;;    (vau (first second) env
;;      ((wrap (vau #ign #ign (eval second env)))
;;       (eval first env)))))

(def list (wrap (vau x #ign x)))

(def list*
  (wrap (vau args #ign
          (begin
            (def aux
              (wrap (vau ((head . tail)) #ign
                      (if (null? tail)
			  head
			  (cons head (aux tail))))))
	    (aux args)))))

(def vau
  ((wrap (vau (vau) #ign
           (vau (formals eformal . body) env
             (eval (list vau formals eformal (cons begin body)) env))))
   vau))

(def lambda
  (vau (formals . body) env
    (wrap (eval (list* vau formals #ign body) env))))

(def car (lambda ((x . #ign)) x))
(def cdr (lambda ((#ign . x)) x))
(def caar (lambda (((x . #ign) . #ign)) x))
(def cadr (lambda ((#ign . (x . #ign))) x))
(def cdar (lambda (((#ign . x) . #ign)) x))
(def cddr (lambda ((#ign . (#ign . x))) x))

(def map (lambda (f l) (if (null? l) () (cons (f (car l)) (map f (cdr l))))))

(def for-each (lambda (f l) (if (null? l) #void (begin (f (car l)) (for-each f (cdr l))))))

(def let
  (vau (bindings . body) env
    (eval (cons (list* lambda (map car bindings) body)
		(map cadr bindings))
	  env)))

(def let*
  (vau (bindings . body) env
    (eval (if (null? bindings)
	      (list* let bindings body)
	      (list let
		    (list (car bindings))
		    (list* let* (cdr bindings) body)))
	  env)))

(def letrec
  (vau (bindings . body) env
    (eval (list* let ()
		 (list def
		       (map car bindings)
		       (list* list (map cadr bindings)))
		 body)
	  env)))

(def apply
  (lambda (appv arg . opt)
    (eval (cons (unwrap appv) arg)
	  (if (null? opt)
	      (make-environment)
	      (car opt)))))

(def cond
  (vau clauses env
    (def aux
      (lambda ((test . body) . clauses)
	(if (eval test env)
	    (apply (wrap begin) body env)
	    (apply (wrap cond) clauses env))))
    (if (null? clauses)
	#void
	(apply aux clauses))))

(def assert (vau (expr) e (if (eval expr e) #void (fail expr))))

(def not (lambda (val) (if val #f #t)))

(def when (vau (test . body) env (eval (list if test (list* begin body) #void) env)))
(def unless (vau (test . body) env (eval (list* when (list not test) body) env)))

(def set!
   (vau (env lhs rhs) denv
      (eval (list def lhs
                  (list (unwrap eval) rhs denv))
            (eval env denv))))

(def provide
  (vau (symbols . body) env
    (eval (list def symbols
		(list let ()
		      (list* begin body)
		      (list* list symbols)))
	  env)))

(def current-environment (vau #ign e e))

(def quote (vau (x) #ign x))

(def define
  (vau (lhs . rhs) env
    (if (pair? lhs)
	(let (((name . args) lhs))
	  (eval (list def name (list* lambda args rhs)) env))
	(eval (list* def lhs rhs) env))))

(def define-syntax
  (vau (lhs . rhs) env
    (if (pair? lhs)
	(let (((name . args) lhs))
	  (eval (list def name (list* vau args (car rhs) (cdr rhs))) env))
	(eval (list* def lhs rhs) env))))

(define-syntax (let-loop a . b) env
  (cond ((pair? a) (eval (list* let a b) env))
	((null? a) (eval (list* let a b) env))
	((symbol? a)
	 (let (((bindings . body) b))
	   (eval (list letrec (list (list a (list* lambda (map car bindings) body)))
		       (list* a (map cadr bindings)))
		 env)))
	(#t (fail "let: not a symbol or list"))))

(define (assq obj alist)
  (if (null? alist) () (if (eq? obj (caar alist)) (car alist) (assq obj (cdr alist)))))

(provide (define-method define-generic send)
  (define (put-method type name method)
    (eval (list def name method) (type-environment type)))
  (define (find-method type name)
    (eval name (type-environment type)))
  (define (send obj message arg)
    (apply (find-method (type-of obj) message) (list* obj arg)))
  (define-syntax (define-generic (name . #ign)) env
    (eval (list def name (lambda (self . arg) (send self name arg))) env))
  (define-syntax (define-method (name (self type) . args) . body) env
    (put-method (eval type env) name (eval (list* lambda (list* self args) body) env)))
)

(define-generic (to-string obj -> string))

(define String (type-of "foo"))
(define (string? s) (eq? (type-of s) String))
(define-method (to-string (self String)) self)

(define (newline) (display "newline")) ; huh?

(provide (make-prompt push-prompt take-sub-cont push-sub-cont shift)
  (define prompt-type (make-type))
  (define (make-prompt) (tag prompt-type #void))
  (define-syntax (push-prompt p . es) env
    (push-prompt* (eval p env) (lambda () (eval (list* begin es) env))))
  (define-syntax (take-sub-cont p k . body) env
    (take-sub-cont* (eval p env) (eval (list* lambda (list k) body) env)))
  (define-syntax (push-sub-cont k . es) env
    (push-sub-cont* (eval k env) (lambda () (eval (list* begin es) env))))
  (define (shift* p f)
    (take-sub-cont p sk (push-prompt p (f (reifyP p sk)))))
  (define (reifyP p sk)
    (lambda (v) (push-prompt p (push-sub-cont sk v))))
  (define-syntax (shift p sk . es) env
    (eval (list shift* p (list* lambda (list sk) es)) env))
)

(provide (dnew dref dlet dlet*)
  (define parameter-type (make-type))
  (define (dnew) (tag parameter-type #void))
  (define (dref p) (shift p sk (lambda (y) ((sk y) y))))
  (define (dlet* p val thunk)
    ((push-prompt p
       (let ((r (thunk)))
         (lambda (y) r)))
     val))
  (define-syntax (dlet key val . body) env
    (eval (list dlet* key val (list* lambda () body)) env))
)

(provide (run yield dynamic-wind for*)
  (define yield-record-type (make-type))
  (define (make-yield-record v k)
    (tag yield-record-type (list v k)))
  (define (try-yield* exp on-r on-y)
    (if (eq? (type-of exp) yield-record-type)
	(let (((v k) (untag exp))) (on-y v k))
	(on-r exp)))
  (define yield-prompt (make-prompt))
  (define-syntax (run e) env (push-prompt* yield-prompt (eval (list lambda () e) env)))
  (define (yield v) (shift yield-prompt k (make-yield-record v k)))
  (define (dynamic-wind before-thunk thunk after-thunk)
    (let-loop loop ((th (lambda (run-thunk))))
      (before-thunk)
      (let ((res (th)))
	(after-thunk)
	(try-yield* res
		    (lambda (r) r)
		    (lambda (v k)
		      (let ((reenter (yield v)))
			(loop (lambda () (k reenter)))))))))
  (define (for* gen body)
    (let-loop loop ((thr (run (gen))))
      (try-yield* thr
		  (lambda (r) r)
		  (lambda (v k)
		    (body v)
		    (loop (k #f))))))
)

(define *top-level* (make-prompt))

