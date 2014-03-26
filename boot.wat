;; -*- mode: scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename ur-define
(vm-def _define vm-def)

;; Rename bindings that will be used as provided by VM
(_define array-to-list vm-array-to-list)
(_define begin vm-begin)
(_define cons vm-cons)
(_define cons? vm-cons?)
(_define dnew vm-dnew)
(_define dref vm-dref)
(_define error vm-error)
(_define eval vm-eval)
(_define if vm-if)
(_define js-getter vm-js-getter)
(_define js-global vm-js-global)
(_define js-invoker vm-js-invoker)
(_define list* vm-list*)
(_define list-to-array vm-list-to-array)
(_define make-environment vm-make-environment)
(_define new vm-js-new)
(_define nil? vm-nil?)
(_define reverse-list vm-reverse-list)
(_define setter vm-setter)
(_define symbol-name vm-symbol-name)
(_define symbol? vm-symbol?)
(_define throw vm-throw)
(_define unwrap vm-unwrap)
(_define wrap vm-wrap)

;; Important utilities
(_define quote (vm-vau (x) #ignore x))
(_define list (wrap (vm-vau elts #ignore elts)))
(_define the-environment (vm-vau () e e))

;;;; Macro and vau

(_define make-macro-expander
  (wrap
    (vm-vau (expander) #ignore
      (vm-vau operands env
        (eval (eval (cons expander operands) (make-environment)) env)))))

(_define _vau
  (make-macro-expander
    (vm-vau (params env-param . body) #ignore
      (list vm-vau params env-param (list* begin body)))))

(_define macro
  (make-macro-expander
    (_vau (params . body) #ignore
      (list make-macro-expander (list* _vau params #ignore body)))))

(_define define-macro
  (macro ((name . params) . body)
    (list _define name (list* macro params body))))

(define-macro (define-operative (name . params) envparam . body)
  (list _define name (list* _vau params envparam body)))

(define-macro (_lambda params . body)
  (list wrap (list* _vau params #ignore body)))

;;;; Wrap incomplete VM forms

(define-macro (loop . body)
  (list vm-loop (list* begin body)))

(define-operative (catch protected handler) env
  (eval (list vm-catch protected (eval handler env)) env))

(define-operative (push-prompt prompt . body) env
  (eval (list vm-push-prompt (eval prompt env) (list* begin body)) env))

(define-macro (take-subcont prompt k . body)
  (list vm-take-subcont prompt (list* _lambda (list k) body)))

(define-macro (push-subcont k . body)
  (list vm-push-subcont k (list* _lambda () body)))

;;;; List utilities

(_define compose (_lambda (f g) (_lambda (arg) (f (g arg)))))

(_define car (_lambda ((x . #ignore)) x))
(_define cdr (_lambda ((#ignore . x)) x))
(_define caar (compose car car))
(_define cadr (compose car cdr))
(_define cdar (compose cdr car))
(_define cddr (compose cdr cdr))

;;;; Important macros and functions

(_define map-list
  (_lambda (f lst)
    (if (nil? lst)
        ()
        (cons (f (car lst)) (map-list f (cdr lst))))))

(_define fold-list
  (_lambda (f init lst)
    (if (nil? lst)
        init
        (fold-list f (f init (car lst)) (cdr lst)))))

(define-macro (let bindings . body)
  (cons
   (list* _lambda (map-list car bindings) body)
   (map-list cadr bindings)))

(define-macro (let* bindings . body)
  (if (nil? bindings)
      (list* let () body)
      (list let (list (car bindings)) (list* let* (cdr bindings) body))))

(define-macro (lambda params . body)
  (_define typed-params->names-and-checks
    (_lambda (ps)
      (if (cons? ps)
          (let* (((p . rest-ps) ps)
                 ((names . checks) (typed-params->names-and-checks rest-ps)))
            (if (cons? p)
                (let* (((name type) p)
                       (check (list the type name)))
                  (cons (cons name names) (cons check checks)))
                (cons (cons p names) checks)))
          (cons ps ()))))
  (let (((untyped-names . type-checks) (typed-params->names-and-checks params)))
    (list* _lambda untyped-names (list* begin type-checks) body)))

(define-macro (define lhs . rhs)
  (if (cons? lhs)
    (list _define (car lhs) (list* lambda (cdr lhs) rhs))
    (list _define lhs (car rhs))))

(define (apply appv arg . opt)
  (if (instanceof appv $Function)
      (~apply appv #null (list-to-array arg))
      (eval (cons (unwrap appv) arg)
            (if (nil? opt)
                (make-environment)
                (car opt)))))

;;;; Simple control

(define-operative (cond . clauses) env
  (if (nil? clauses)
      #undefined
      (let ((((test . body) . clauses) clauses))
        (if (eval test env)
            (apply (wrap begin) body env)
            (apply (wrap cond) clauses env)))))

(define else #t)

(define-operative (and . x) e
  (cond ((nil? x)         #t)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(define-operative (or . x) e
  (cond ((nil? x)         #f)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(define (call-with-escape fun)
  (let ((fresh (list #null)))
    (catch (fun (_lambda opt-arg (throw (list fresh opt-arg))))
      (_lambda (exc)
        (if (and (cons? exc) (= fresh (car exc)))
            (let ((opt-arg (cadr exc)))
              (if (cons? opt-arg) (car opt-arg) #undefined))
            (throw exc))))))

(define-macro (label name . body)
  (list call-with-escape (list* _lambda (list name) body)))

(define (call-while test-fun body-fun)
  (label return
    (loop
      (if (test-fun)
        (body-fun)
        (return)))))

(define-macro (while test . body)
  (list call-while
        (list _lambda () test)
        (list* _lambda () body)))

(define-macro (when test . body)
  (list if test (list* begin body) #null))

(define-macro (unless test . body)
  (list* when (list not test) body))

(define-macro (set! (getter . args) new-val)
  (list* (list setter getter) new-val args))

;;;; Delimited dynamic binding

;; Evaluate right hand sides before binding all dynamic variables at once.
(define-operative (dlet bindings . body) env
  (define (process-bindings bs)
    (if (nil? bs)
        (list* begin body)
        (let* ((((name expr) . rest-bs) bs)
               (value (eval expr env)))
          (list vm-dlet name value (process-bindings rest-bs)))))
  (eval (process-bindings bindings) env))

;;;; Prototypes

(define-operative (define-prototype name super prop-names) env
  (let ((p (apply vm-js-make-prototype (list* (symbol-name name) (map-list symbol-name prop-names)))))
    (set! (.prototype (.constructor p)) (new (eval super env)))
    (eval (list _define name p) env)))

(define (put-method ctor name js-fun)
  (set! ((js-getter name) (.prototype ctor)) js-fun))

(define-macro (define-method (name (self ctor) . args) . body)
  (list put-method ctor (symbol-name name)
        (list vm-js-function (list* lambda (list* self args) body))))

(define-macro (define-generic (name . #ignore))
  (list _define name (vm-js-invoker (symbol-name name))))

;;;; Modules

(define-operative (provide symbols . body) env
  (eval (list _define symbols
              (list let ()
                    (list* begin body)
                    (list* list symbols)))
        env))

(define-operative (module exports . body) env
  (let ((menv (make-environment env)))
    (eval (list* provide exports body) menv)
    (make-environment menv)))

(define-macro (define-module name exports . body)
  (list _define name (list* module exports body)))

(define-operative (import module imports) env
  (let* ((m (eval module env))
         (values (map-list (_lambda (import) (eval import m)) imports)))
    (eval (list _define imports (list* list values)) env)))

;;;; JavaScript

(define (relational-js-binop name)
  (let ((binop (vm-js-binop name)))
    (define (fun arg1 arg2 . rest)
      (if (binop arg1 arg2)
          (if (nil? rest)
              #t
              (apply fun (list* arg2 rest)))
          #f))
    fun))

(define = (relational-js-binop "==="))
(define < (relational-js-binop "<"))
(define > (relational-js-binop ">"))
(define <= (relational-js-binop "<="))
(define >= (relational-js-binop ">="))

(define (!= . args) (not (apply = args)))

(define * (let ((vm* (vm-js-binop "*")))
            (lambda args
              (fold-list vm* 1 args))))

;; Can't simply use 0 as unit or it won't work with strings
(define + (let ((vm+ (vm-js-binop "+")))
            (lambda args
              (if (nil? args)
                  0
                  (fold-list vm+ (car args) (cdr args))))))

(define (folding-js-op-neg binop unit)
  (lambda (arg1 . rest)
    (if (nil? rest)
        (binop unit arg1)
        (fold-list binop arg1 rest))))

(define - (folding-js-op-neg (vm-js-binop "-") 0))
(define / (folding-js-op-neg (vm-js-binop "/") 1))

(define not (vm-js-unop "!"))
(define typeof (vm-js-unop "typeof"))
(define % (vm-js-binop "%"))
(define in (vm-js-binop "in"))
(define instanceof (vm-js-binop "instanceof"))

(define bitand (vm-js-binop "&"))
(define bitor (vm-js-binop "|"))
(define bitxor (vm-js-binop "^"))
(define bitnot (vm-js-unop "~"))
(define bitshiftl (vm-js-binop "<<"))
(define bitshiftr (vm-js-binop ">>"))
(define bitshiftr0 (vm-js-binop ">>>"))

(define-operative (object . pairs) env
  (let ((obj (vm-js-make-object)))
    (map-list (_lambda ((name value))
                (set! ((js-getter (eval name env)) obj) (eval value env)))
              pairs)
    obj))

(define (@ object key)
  ((js-getter key) object))

(set! (setter @) (lambda (new-val object key)
                   (set! ((js-getter key) object) new-val)))

(define (array . args) (list-to-array args))

(define (js-callback fun)
  (vm-js-function (_lambda args (push-prompt vm-root-prompt (apply fun args)))))

(define-macro (type? obj type)
  (list vm-type? obj type (symbol-name type)))

(define-macro (the type obj)
  (list if (list type? obj type) obj (list error (list + obj " is not a: " type))))

(define Arguments $Arguments)
(define Array $Array)
(define Boolean $Boolean)
(define Date $Date)
(define Function $Function)
(define Number $Number)
(define Object $Object)
(define RegExp $RegExp)
(define String $String)

(define (log x . xs)
  (apply ~log (list* $console x xs))
  x)

;;;; Error break routine, called by VM to print stacktrace and throw

(define (user-break err)
  (define (print-frame k)
    (log (~toString (.dbg k)) (.e k))
    (if (.next k)
        (print-frame (.next k))
        #undefined))
  (take-subcont vm-root-prompt k
    (print-frame k)
    (push-prompt vm-root-prompt
      (push-subcont k
        (throw err)))))

;;;; Final events

(define-operative (let-redirect exp bindings . body) env
  (eval (list* (eval (list* _lambda (map-list car bindings) body)
                     (eval exp
                           env))
               (map-list cadr bindings))
        env))

(define-operative (bindings->environment . bindings) denv
  (eval (list let-redirect
              (make-environment)
              bindings
              (list the-environment))
        denv))

(define-operative (slurp-environment . bindings) env
  (eval (list* bindings->environment (map-list (lambda (b) (list b b)) bindings)) env))

;;;; Export bindings to userland

;; User environment is subenvironment of environment containing exported bindings
;; so exported bindings cannot be modified

(make-environment 
  (slurp-environment 
   define-operative _define _lambda _vau apply eval make-environment the-environment unwrap wrap
   begin define define-macro lambda let let* quote symbol-name symbol?
   caar cadr car cdar cddr cdr cons cons? fold-list list list* map-list nil? reverse-list
   define-generic define-prototype define-method new the type?
   catch cond else if label loop throw unless when while error 
   set! setter
   push-prompt push-subcont take-subcont
   dlet dnew dref
   define-module import module
   Arguments Array Date Function Number Object RegExp String
   array array-to-list js-callback js-getter js-global js-invoker list-to-array object log
   @ and or not != % * + - / < <= = > >= in instanceof typeof
   bitand bitor bitxor bitnot bitshiftl bitshiftr bitshiftr0
   ))