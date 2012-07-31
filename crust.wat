;; -*- mode: scheme -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def null? (wrap (vau (val) #ign (eq? () val))))

(def pair? (wrap (vau (val) #ign (eq? (type-of val) (type-of (cons #void #void))))))

(def symbol? (wrap (vau (val) #ign (eq? (type-of val) (type-of 'foo)))))

(def procedure? (wrap (vau (val) #ign (eq? (type-of val) (type-of (wrap (vau #ign #ign #void)))))))

(def begin
  ((wrap (vau (seq2) #ign
	   (seq2
             (def aux
               (vau (head . tail) env
		 (if (null? tail)
		     (eval head env)
		     (seq2 (eval head env) (eval (cons aux tail) env)))))
             (vau body env
               (if (null? body)
                   #void
                   (eval (cons aux body) env))))))
   (vau (first second) env
     ((wrap (vau #ign #ign (eval second env)))
      (eval first env)))))

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

(def scope
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

(define (assq obj alist)
  (if (null? alist) () (if (eq? obj (caar alist)) (car alist) (assq obj (cdr alist)))))

(scope (call/cc dynamic-wind call-with-current-continuation)

  (define *env* (current-environment))

  (define *winds* ())

  (define (dynamic-wind <thunk1> <thunk2> <thunk3>)
    (<thunk1>)
    (set! *env* *winds* (cons (cons <thunk1> <thunk3>) *winds*))
    (let ((ans (<thunk2>)))
      (set! *env* *winds* (cdr *winds*))
      (<thunk3>)
      ans))
 
  (define (non-winding-call/cc f)
    (ccc (lambda (k) (f (lambda (val) (jump k val))))))

  (define (call-with-current-continuation proc)
    (let ((winds *winds*))
      (non-winding-call/cc
       (lambda (cont)
	 (proc (lambda (c2)
		 (dynamic:do-winds *winds* winds)
		 (cont c2)))))))

  (define call/cc call-with-current-continuation)

  (define (dynamic:do-winds from to)
    (set! *env* *winds* from)
    (cond ((eq? from to)
	   #void)
	  ((null? from)
	   (dynamic:do-winds from (cdr to))
	   ((caar to)))
	  ((null? to)
	   ((cdar from))
	   (dynamic:do-winds (cdr from) to))
	  (#t
	   ((cdar from))
	   (dynamic:do-winds (cdr from) (cdr to))
	   ((caar to))))
    (set! *env* *winds* to))

)

(define (newline) (display "newline")) ; huh?

