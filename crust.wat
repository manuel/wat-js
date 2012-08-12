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

(provide (make-prompt push-prompt take-sub-cont push-sub-cont shift)
  (def (prompt-type tag-prompt #ign) (make-type))
  (define (make-prompt) (tag-prompt #void))
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
  (def (parameter-type tag-parameter #ign) (make-type))
  (define (dnew) (tag-parameter #void))
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
  (def (yield-record-type tag-yield-record untag-yield-record) (make-type))
  (define (make-yield-record v k)
    (tag-yield-record (list v k)))
  (define (try-yield* exp on-r on-y)
    (if (eq? (type-of exp) yield-record-type)
	(let (((v k) (untag-yield-record exp))) (on-y v k))
	(on-r exp)))
  (define yield-prompt (make-prompt))
  (define-syntax (run e) env (push-prompt* yield-prompt (eval (list lambda () e) env)))
  (define (yield v) (shift yield-prompt k (make-yield-record v k)))
  (define (dynamic-wind before-thunk thunk after-thunk)
    (let-loop loop ((th (lambda () (run-thunk))))
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

(provide (make-hashtable hashtable-put! hashtable-get make-identity-hashtable)
  (define-record-type hashtable
    (construct-hashtable hashfn eqfn env)
    hashtable?
    (hashfn hashfn)
    (eqfn eqfn)
    (env env))
  (define (make-hashtable hashfn eqfn)
    (construct-hashtable hashfn eqfn (make-environment)))
  (define (hashtable-put! ht k v)
    (define hash (string->symbol (number->string ((hashfn ht) k))))
    (if (bound? hash (env ht))
        (let ((buckets (eval hash (env ht))))
          (eval (list def hash (buckets-add/replace buckets k v (eqfn ht)))
                (env ht)))
        (eval (list def hash (list list (list cons k v))) (env ht)))
    v)
  (define (buckets-add/replace buckets k v eqfn)
    (if (null? buckets)
        (list (cons k v))
        (if (eqfn k (caar buckets))
            (list* (cons k v) (cdr buckets))
            (list* (car buckets) (buckets-add/replace (cdr buckets) k v eqfn)))))
  (define (hashtable-get ht k . opt)
    (define hash (string->symbol (number->string ((hashfn ht) k))))
    (define default (if (null? opt) () (car opt)))
    (if (bound? hash (env ht))
        (let ((buckets (eval hash (env ht))))
          (buckets-find buckets k (eqfn ht) default))
        default))
  (define (buckets-find buckets k eqfn default)
    (if (null? buckets)
        default
        (if (eqfn k (caar buckets))
            (cdar buckets)
            (buckets-find (cdr buckets) k eqfn default))))
  (define (make-identity-hashtable) (make-hashtable identity-hash-code eq?))
)

(provide (define-generic define-method put-method!)
  (define generic->vtable (make-identity-hashtable))
  (define-syntax (define-generic (name . args) . body) env
    (define vtable (make-identity-hashtable))
    (define default-method (if (null? body)
                               (lambda #ign (fail "method not found"))
                               (eval (list* lambda args body) env)))
    (define (generic self . arg)
      (apply (hashtable-get vtable (type-of self) default-method) (cons self arg)))
    (eval (list def name generic) env)
    (hashtable-put! generic->vtable generic vtable)
    generic)
  (define (put-method! generic type method)
    (define vtable (hashtable-get generic->vtable generic))
    (hashtable-put! vtable type method))
  (define-syntax (define-method (name (self type) . args) . body) env
    (define method (eval (list* lambda (list* self args) body) env))
    (put-method! (eval name env) (eval type env) method))
)

(define String (type-of "foo"))
(define Symbol (type-of 'foo))
(define Number (type-of 0))

(provide (=)
  (define-generic (= a b) (eq? a b))
  (define-syntax (define-builtin-= type-name pred-expr) env
    (define type (eval type-name env))
    (define pred (eval pred-expr env))
    (put-method! = type (lambda (a b) (if (eq? type (type-of b)) (pred a b) #f))))
  (define-builtin-= Number num=)
  (define-builtin-= String str=)
  (define-builtin-= Symbol (lambda (a b) (= (symbol->string a) (symbol->string b))))
)

(provide (hash-code)
  (define-generic (hash-code obj) (identity-hash-code obj))
)
