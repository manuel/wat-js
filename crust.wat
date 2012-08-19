;; -*- mode: scheme -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

(def quote (vau (x) #ign x))

(def Void (type-of #void))
(def Ign (type-of #ign))
(def Boolean (type-of #t))
(def Nil (type-of ()))
(def Pair (type-of (cons #void #void)))
(def Symbol (type-of 'foo))
(def String (type-of "foo"))
(def Number (type-of 0))
(def Applicative (type-of (wrap (vau #ign #ign #void))))
(def Operative (type-of (vau #ign #ign #void)))
(def Environment (type-of (make-environment)))
(def Vector (type-of (vector)))
(def Type (type-of (make-type)))

(def void? (wrap (vau (val) #ign (eq? #void val))))
(def ign? (wrap (vau (val) #ign (eq? #ign val))))
(def boolean? (wrap (vau (val) #ign (eq? (type-of val) Boolean))))
(def null? (wrap (vau (val) #ign (eq? () val))))
(def pair? (wrap (vau (val) #ign (eq? (type-of val) Pair))))
(def symbol? (wrap (vau (val) #ign (eq? (type-of val) Symbol))))
(def string? (wrap (vau (val) #ign (eq? (type-of val) String))))
(def symbol? (wrap (vau (val) #ign (eq? (type-of val) Symbol))))
(def number? (wrap (vau (val) #ign (eq? (type-of val) Number))))
(def applicative? (wrap (vau (val) #ign (eq? (type-of val) Applicative))))
(def operative? (wrap (vau (val) #ign (eq? (type-of val) Operative))))
(def environment? (wrap (vau (val) #ign (eq? (type-of val) Environment))))
(def vector? (wrap (vau (val) #ign (eq? (type-of val) Vector))))
(def type? (wrap (vau (val) #ign (eq? (type-of val) Type))))

;; (def begin
;;    ((wrap (vau (seq2) #ign
;;             (seq2
;;               (def aux
;;                 (vau (head . tail) env
;;                   (if (null? tail)
;;                       (eval head env)
;;                       (seq2
;;                         (eval head env)
;;                         (eval (cons aux tail) env)))))
;;                (vau body env
;;                  (if (null? body)
;;                      #void
;;                      (eval (cons aux body) env))))))
;;       (vau (first second) env
;;          ((wrap (vau #ign #ign (eval second env)))
;;           (eval first env)))))

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
(def map2 (lambda (f l1 l2)
            (if (null? l1)
                ()
                (if (null? l2)
                    ()
                    (cons (f (car l1) (car l2)) (map2 f (cdr l1) (cdr l2)))))))

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

(def or (vau (a b) env (if (eval a env) #t (eval b env))))

(def and (vau (a b) env (if (eval a env) (eval b env) #f)))

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

(def define
  (vau (lhs . rhs) env
    (if (pair? lhs)
	(let* (((name . args) lhs)
               (proc (eval (list* lambda args rhs) env)))
	  (eval (list def name proc) env)
          (set-label! proc (symbol->string name)))
	(eval (list* def lhs rhs) env))))

(def define-syntax
  (vau (lhs . rhs) env
    (if (pair? lhs)
	(let* (((name . args) lhs)
               (opv (eval (list* vau args (car rhs) (cdr rhs)) env)))
	  (eval (list def name opv) env)
          (set-label! opv (symbol->string name)))
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

(define (sublist l i) (if (<= i 0) l (sublist (cdr l) (- i 1))))

(define (assq obj alist)
  (if (null? alist) () (if (eq? obj (caar alist)) (car alist) (assq obj (cdr alist)))))

(define-syntax (define-record-type name (ctor-name . ctor-field-names) pred-name . field-specs) env
  (let* (((type tagger untagger) (make-type))
         (ctor (lambda ctor-args
                 (let ((fields-dict (make-environment)))
                   (map2 (lambda (field-name arg)
                           (eval (list def field-name arg) fields-dict))
                         ctor-field-names
                         ctor-args)
                   (tagger fields-dict))))
         (pred (lambda (obj) (eq? (type-of obj) type))))
    (eval (list def (list name ctor-name pred-name) (list list type ctor pred)) env)
    (set-label! type (symbol->string name))
    (map (lambda (field-spec)
           (let (((name accessor-name . opt) field-spec))
             (eval (list def accessor-name (lambda (obj)
                                             (let ((fields-dict (untagger obj)))
                                               (eval name fields-dict))))
                   env)
             (unless (null? opt)
               (let (((modifier-name) opt))
                 (eval (list def modifier-name (lambda (obj new-val)
                                                 (let ((fields-dict (untagger obj)))
                                                   (eval (list def name new-val) fields-dict))))
                       env)))))
         field-specs)
    type))

(define *top-level* '*top-level*)

(define (trap exc)
  (display (strcat "An error occurred: " (->string (from-js exc))))
  (display (strcat "Stacktrace:"))
  (print-stacktrace 20)
  (take-sub-cont *top-level* k (push-prompt *top-level* #void)))

