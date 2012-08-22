;; -*- mode: scheme -*-
;; This is the hard crust of Wat code around the JS core defined in `wat.js`.

;(start-profile)

(def quote (vau (x) #ign x))
(set-label! quote "quote")

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
(set-label! list "list")

;; (def list*
;;   (wrap (vau args #ign
;;           (begin
;;             (def aux
;;               (wrap (vau ((head . tail)) #ign
;;                       (if (null? tail)
;; 			  head
;; 			  (cons head (aux tail))))))
;; 	    (aux args)))))
;; (set-label! list* "list*")

(def vau
  ((wrap (vau (vau) #ign
           (vau (formals eformal . body) env
             (eval (list vau formals eformal (cons begin body)) env))))
   vau))
(set-label! vau "vau")

(def lambda
  (vau (formals . body) env
    (wrap (eval (list* vau formals #ign body) env))))
(set-label! lambda "lambda")

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

(define-syntax (dlet dv val . exprs) env
  (eval (list dlet* dv val (list* lambda () exprs)) env))

(define-syntax (coroutine . exprs) env
  (coroutine* (eval (list* lambda () exprs) env)))

;; (define-syntax (block name . exprs) env
;;   (letrec ((aborter (lambda (val) (throw aborter val))))
;;     (eval (list* let (list (list name aborter))
;;             (list* catch aborter exprs))
;;           env)))

(provide (define-generic define-method)
  (define-syntax (define-generic (name . args) . body) env
    (define str-name (symbol->string name))
    (define default-method (if (null? body)
                               (lambda #ign (fail (strcat "method not found: " str-name)))
                               (eval (list* lambda args body) env)))
    (define (generic self . arg)
      (apply (find-method (type-of self) str-name default-method) (cons self arg)))
    (set-label! generic str-name)
    (eval (list def name generic) env)
    generic)
  (define-syntax (define-method (name (self type) . args) . body) env
    (define method (eval (list* lambda (list* self args) body) env))
    (put-method! (eval type env) (symbol->string name) method))
)

(provide (= /=)
  (define-generic (= a b) (eq? a b))
  (define-syntax (define-builtin-= type-name pred-expr) env
    (define type (eval type-name env))
    (define pred (eval pred-expr env))
    (put-method! type "=" (lambda (a b) (if (eq? type (type-of b)) (pred a b) #f))))
  (define-builtin-= Number num=)
  (define-builtin-= String str=)
  (define-builtin-= Symbol (lambda (a b) (= (symbol->string a) (symbol->string b))))
  (define (/= a b) (not (= a b)))
)

(provide (< > <= >=)
  (define-generic (< a b))
  (define-method (< (a Number) b) (if (number? b) (num< a b) (fail "can't compare number")))
  (define (> a b) (< b a))
  (define (<= a b) (or (< a b) (= a b)))
  (define (>= a b) (or (> a b) (= a b)))
)

(provide (hash-code)
  (define-generic (hash-code obj) (identity-hash-code obj))
)

(provide (->string)
  (define-generic (->string obj) (strcat "#[" (label obj) "]"))
  (define-method (->string (obj Void)) "#void")
  (define-method (->string (obj Ign)) "#ign")
  (define-method (->string (obj Boolean)) (if obj "#t" "#f"))
  (define-method (->string (obj Nil)) "()")
  (define-method (->string (obj Pair))
    (define (pair->string (kar . kdr))
      (if (null? kdr)
          (->string kar)
          (if (pair? kdr)
              (strcat (->string kar) " " (pair->string kdr))
              (strcat (->string kar) " . " (->string kdr)))))
    (strcat "(" (pair->string obj) ")"))
  (define-method (->string (obj Symbol)) (symbol->string obj))
  (define-method (->string (obj String)) (str-print obj))
  (define-method (->string (obj Number)) (number->string obj))
  (define-method (->string (obj Applicative)) (strcat "#[Applicative " (label obj) "]"))
  (define-method (->string (obj Operative)) (strcat "#[Operative " (label obj) "]"))
  (define-method (->string (obj Environment)) "#[Environment]")
  (define-method (->string (obj Vector)) "#[Vector]")
)

(define (display-stacktrace trace)
  (for-each (lambda (frame) (display (->string frame))) trace))

(define (trap exc)
  (define trace (stacktrace))
  (display "ERROR")
  (display exc)
  (display-stacktrace trace))

(provide (make-prompt push-prompt take-sub-cont push-sub-cont shift)
  (def (prompt-type tag-prompt #ign) (make-type))
  (define (make-prompt) (tag-prompt #void))
  (define-syntax (push-prompt p . es) env
    (push-prompt* (eval p env) (eval (list* lambda () es) env)))
  (define-syntax (take-sub-cont p k . body) env
    (take-sub-cont* (eval p env) (eval (list* lambda (list k) body) env)))
  (define-syntax (push-sub-cont k . es) env
    (push-sub-cont* (eval k env) (eval (list* lambda () es) env)))
  (define (shift* p f)
    (take-sub-cont p sk (push-prompt p (f (reifyP p sk)))))
  (define (reifyP p sk)
    (lambda (v) (push-prompt p (push-sub-cont sk v))))
  (define-syntax (shift p sk . es) env
    (eval (list shift* p (list* lambda (list sk) es)) env))
)

