;; -*- mode: scheme -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def null? (wrap (vau (val) #ign (eq () val))))

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
(def cadr (lambda (#ign . (x . #ign)) x))

(def map (lambda (f l) (if (null? l) () (cons (f (car l)) (map f (cdr l))))))

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

(def call/cc (lambda (f) (ccc (lambda (k) (f (lambda (val) (jump k val)))))))

(def assert (vau (expr) e (if (eval expr e) #void (fail expr))))

(def not (lambda (val) (if val #f #t)))

(def when (vau (test . body) env (eval (list if test (list* begin body) #void) env)))
(def unless (vau (test . body) env (eval (list* when (list* not test) body) env)))

(def set!
  (vau (env lhs rhs) denv
     (eval (list def lhs (list (unwrap eval) rhs env))
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

;;;;; DELIMITED CONTROL

(scope ()

  (def env (current-environment))
  (def pstack ())
  (def go #f)

  (let ((v (call/cc (lambda (k) (set! env go k) (k #f)))))
    (when v
      (let* ((r (v))
	     (h (car pstack))
	     (_ (set! env pstack (cdr pstack))))
	((cdr h) (lambda () r)))))

  (def new-prompt (lambda () (list #f)))

  (def push-prompt*
    (lambda (p th)
      ((call/cc
	(lambda (k)
	  (set! env pstack (cons (cons p k) pstack))
	  (go th))))))

  (def unwind
     (lambda (acc p pstack)
       (if (null? pstack) (fail "No prompt was set")
	   (if (eq p (caar pstack))
	       (cons pstack acc)
	       (unwind (cons (car pstack) acc) p (cdr pstack))))))
  
  (def unwind-abort
    (lambda (p pstack)
      (if (null? pstack) (fail "No prompt was set")
	  (if (eq? p (caar pstack))
	      pstack
	      (unwind-abort p (cdr pstack))))))

  (def take-SC
    (lambda (p f)
      ((call/cc
	(lambda (k)
	  (let* ((subchain-pstack (unwind '() p pstack))
		 (_ (set! pstack (car subchain-pstack)))
		 (subchain (cdr subchain-pstack)))
	    (go (f (vector k p subchain)))))))))
    
  