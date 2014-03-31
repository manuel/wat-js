;; -*- mode: lisp -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename ur-def
(vm-def _def vm-def)

;; Rename bindings that will be used as provided by VM
(_def array-to-list vm-array-to-list)
(_def begin vm-begin)
(_def cons vm-cons)
(_def cons? vm-cons?)
(_def dnew vm-dnew)
(_def dref vm-dref)
(_def error vm-error)
(_def eval vm-eval)
(_def if vm-if)
(_def js-getter vm-js-getter)
(_def js-global vm-js-global)
(_def js-invoker vm-js-invoker)
(_def list* vm-list*)
(_def list-to-array vm-list-to-array)
(_def make-environment vm-make-environment)
(_def new vm-js-new)
(_def nil? vm-nil?)
(_def reverse-list vm-reverse-list)
(_def setter vm-setter)
(_def symbol-name vm-symbol-name)
(_def symbol? vm-symbol?)
(_def throw vm-throw)
(_def unwrap vm-unwrap)
(_def wrap vm-wrap)

;; Important utilities
(_def quote (vm-vau (x) #ignore x))
(_def list (wrap (vm-vau elts #ignore elts)))
(_def the-environment (vm-vau () e e))

;;;; Macro and vau

(_def make-macro-expander
  (wrap
    (vm-vau (expander) #ignore
      (vm-vau operands env
        (eval (eval (cons expander operands) (make-environment)) env)))))

(_def _vau
  (make-macro-expander
    (vm-vau (params env-param . body) #ignore
      (list vm-vau params env-param (list* begin body)))))

(_def macro
  (make-macro-expander
    (_vau (params . body) #ignore
      (list make-macro-expander (list* _vau params #ignore body)))))

(_def defmacro
  (macro ((name . params) . body)
    (list _def name (list* macro params body))))

(defmacro (_lambda params . body)
  (list wrap (list* _vau params #ignore body)))

(defmacro (defoperative (name . params) envparam . body)
  (list _def name (list* _vau params envparam body)))

;;;; Wrap incomplete VM forms

(defmacro (loop . body)
  (list vm-loop (list* begin body)))

(defoperative (catch protected handler) env
  (eval (list vm-catch protected (eval handler env)) env))

(defoperative (push-prompt prompt . body) env
  (eval (list vm-push-prompt (eval prompt env) (list* begin body)) env))

(defmacro (take-subcont prompt k . body)
  (list vm-take-subcont prompt (list* _lambda (list k) body)))

(defmacro (push-subcont k . body)
  (list vm-push-subcont k (list* _lambda () body)))

(defmacro (push-prompt-subcont p k . body)
  (list vm-push-prompt-subcont p k (list* _lambda () body)))

;;;; List utilities

(_def compose (_lambda (f g) (_lambda (arg) (f (g arg)))))

(_def car (_lambda ((x . #ignore)) x))
(_def cdr (_lambda ((#ignore . x)) x))
(_def caar (compose car car))
(_def cadr (compose car cdr))
(_def cdar (compose cdr car))
(_def cddr (compose cdr cdr))

;;;; Important macros and functions

(_def map-list
  (_lambda (f lst)
    (if (nil? lst)
        ()
        (cons (f (car lst)) (map-list f (cdr lst))))))

(_def fold-list
  (_lambda (f init lst)
    (if (nil? lst)
        init
        (fold-list f (f init (car lst)) (cdr lst)))))

(defmacro (let x . rest)
  (if (symbol? x)
      (list* let-loop x rest)
      (list* (list* _lambda (map-list car x) rest)
             (map-list cadr x))))

(defmacro (let-loop name bindings . body)
  (list letrec (list (list name (list* lambda (map-list car bindings)
                                       body)))
        (list* name (map-list cadr bindings))))

(defmacro (let* bindings . body)
  (if (nil? bindings)
      (list* let () body)
      (list let (list (car bindings))
            (list* let* (cdr bindings) body))))

(defmacro (letrec bindings . body)
  (list* let ()
         (list _def
               (map-list car bindings)
               (list* list (map-list cadr bindings)))
         body))

(defmacro (lambda params . body)
  (letrec ((typed-params->names-and-checks
            (_lambda (ps)
              (if (cons? ps)
                  (let* (((p . rest-ps) ps)
                         ((names . checks) (typed-params->names-and-checks rest-ps)))
                    (if (cons? p)
                        (let* (((name type) p)
                               (check (list the type name)))
                          (cons (cons name names) (cons check checks)))
                        (cons (cons p names) checks)))
                  (cons ps ())))))
    (let (((untyped-names . type-checks) (typed-params->names-and-checks params)))
      (list* _lambda untyped-names (list* begin type-checks) body))))

(defmacro (def lhs . rhs)
  (if (cons? lhs)
    (list _def (car lhs) (list* lambda (cdr lhs) rhs))
    (list _def lhs (car rhs))))

(def (apply appv arg . opt)
  (if (instanceof appv $Function)
      (~apply appv #null (list-to-array arg))
      (eval (cons (unwrap appv) arg)
            (if (nil? opt)
                (make-environment)
                (car opt)))))

;;;; Simple control

(defoperative (cond . clauses) env
  (if (nil? clauses)
      #undefined
      (let ((((test . body) . clauses) clauses))
        (if (eval test env)
            (apply (wrap begin) body env)
            (apply (wrap cond) clauses env)))))

(def else #t)

(defoperative (and . x) e
  (cond ((nil? x)         #t)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(defoperative (or . x) e
  (cond ((nil? x)         #f)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(def (call-with-escape fun)
  (let ((fresh (list #null)))
    (catch (fun (_lambda opt-arg (throw (list fresh opt-arg))))
      (_lambda (exc)
        (if (and (cons? exc) (= fresh (car exc)))
            (let ((opt-arg (cadr exc)))
              (if (cons? opt-arg) (car opt-arg) #undefined))
            (throw exc))))))

(defmacro (label name . body)
  (list call-with-escape (list* _lambda (list name) body)))

(defoperative (while test . body) env
  (let ((body (list* begin body)))
    (label return
      (loop
        (if (eval test env)
          (eval body env)
          (return))))))

(defmacro (when test . body)
  (list if test (list* begin body) #null))

(defmacro (unless test . body)
  (list* when (list not test) body))

(defmacro (set (getter . args) new-val)
  (list* (list setter getter) new-val args))

;;;; Delimited dynamic binding

;; Evaluate right hand sides before binding all dynamic variables at once.
(defoperative (dlet bindings . body) env
  (eval (let process-bindings ((bs bindings))
          (if (nil? bs)
              (list* begin body)
              (let* ((((name expr) . rest-bs) bs)
                     (value (eval expr env)))
                (list vm-dlet name value (process-bindings rest-bs)))))
        env))

;;;; Prototypes

(defoperative (defprototype name super prop-names) env
  (let ((p (apply vm-js-make-prototype (list* (symbol-name name) (map-list symbol-name prop-names)))))
    (set (.prototype (.constructor p)) (new (eval super env)))
    (eval (list _def name p) env)))

(def (put-method ctor name js-fun)
  (set ((js-getter name) (.prototype ctor)) js-fun))

(defmacro (defmethod (name (self ctor) . args) . body)
  (list put-method ctor (symbol-name name)
        (list vm-js-function (list* lambda (list* self args) body))))

(defmacro (defgeneric (name . #ignore))
  (list _def name (vm-js-invoker (symbol-name name))))

;;;; Modules

(defoperative (provide symbols . body) env
  (eval (list _def symbols
              (list let ()
                    (list* begin body)
                    (list* list symbols)))
        env))

(defoperative (module exports . body) env
  (let ((menv (make-environment env)))
    (eval (list* provide exports body) menv)
    (make-environment menv)))

(defmacro (defmodule name exports . body)
  (list _def name (list* module exports body)))

(defoperative (import module imports) env
  (let* ((m (eval module env))
         (values (map-list (_lambda (import) (eval import m)) imports)))
    (eval (list _def imports (list* list values)) env)))

;;;; JavaScript

(def (relational-op name)
  (let ((binop (vm-js-binop name)))
    (letrec ((op (lambda (arg1 arg2 . rest)
                   (if (binop arg1 arg2)
                       (if (nil? rest)
                           #t
                           (apply op (list* arg2 rest)))
                       #f))))
      op)))

(def = (relational-op "==="))
(def < (relational-op "<"))
(def > (relational-op ">"))
(def <= (relational-op "<="))
(def >= (relational-op ">="))

(def (!= . args) (not (apply = args)))

(def * (let ((vm* (vm-js-binop "*")))
         (lambda args
           (fold-list vm* 1 args))))

;; Can't simply use 0 as unit or it won't work with strings
(def + (let ((vm+ (vm-js-binop "+")))
         (lambda args
           (if (nil? args)
               0
               (fold-list vm+ (car args) (cdr args))))))

(def (negative-op binop unit)
  (lambda (arg1 . rest)
    (if (nil? rest)
        (binop unit arg1)
        (fold-list binop arg1 rest))))

(def - (negative-op (vm-js-binop "-") 0))
(def / (negative-op (vm-js-binop "/") 1))

(def % (vm-js-binop "%"))
(def not (vm-js-unop "!"))
(def typeof (vm-js-unop "typeof"))
(def in (vm-js-binop "in"))
(def instanceof (vm-js-binop "instanceof"))

(def bitand (vm-js-binop "&"))
(def bitor (vm-js-binop "|"))
(def bitxor (vm-js-binop "^"))
(def bitnot (vm-js-unop "~"))
(def bitshiftl (vm-js-binop "<<"))
(def bitshiftr (vm-js-binop ">>"))
(def bitshiftr0 (vm-js-binop ">>>"))

(defoperative (object . pairs) env
  (let ((obj (vm-js-make-object)))
    (map-list (_lambda ((name value))
                (set ((js-getter (eval name env)) obj) (eval value env)))
              pairs)
    obj))

(def (@ object key)
  ((js-getter key) object))

(set (setter @) (lambda (new-val object key)
                  (set ((js-getter key) object) new-val)))

(def (array . args) (list-to-array args))

(def (js-callback fun)
  (vm-js-function (_lambda args (push-prompt vm-root-prompt (apply fun args)))))

(defmacro (type? obj type)
  (list vm-type? obj type (symbol-name type)))

(defmacro (the type obj)
  (list if (list type? obj type) obj (list error (list + obj " is not a: " type))))

(def Arguments $Arguments)
(def Array $Array)
(def Boolean $Boolean)
(def Date $Date)
(def Function $Function)
(def Number $Number)
(def Object $Object)
(def RegExp $RegExp)
(def String $String)

(def (log x . xs)
  (apply ~log (list* $console x xs))
  x)

;;;; Error break routine, called by VM to print stacktrace and throw

(def (print-stacktrace)
  (def (print-frame k)
    (log (~toString (.dbg k)) (.e k))
    (if (.next k)
        (print-frame (.next k))
        #undefined))
  (take-subcont vm-root-prompt k
    (print-frame k)
    (push-prompt vm-root-prompt
      (push-subcont k))))

(def (user-break err)
  (print-stacktrace)
  (throw err))

;;;; Final events

(defoperative (let-redirect exp bindings . body) env
  (eval (list* (eval (list* _lambda (map-list car bindings) body)
                     (eval exp
                           env))
               (map-list cadr bindings))
        env))

(defoperative (bindings->environment . bindings) denv
  (eval (list let-redirect
              (make-environment)
              bindings
              (list the-environment))
        denv))

(defoperative (slurp-environment . bindings) env
  (eval (list* bindings->environment (map-list (lambda (b) (list b b)) bindings)) env))

;;;; Export bindings to userland

;; User environment is subenvironment of environment containing exported bindings
;; so exported bindings cannot be modified

(make-environment 
  (slurp-environment 
   defoperative _def _lambda _vau apply eval make-environment the-environment unwrap wrap
   begin def defmacro lambda let let* quote symbol-name symbol?
   caar cadr car cdar cddr cdr cons cons? fold-list list list* map-list nil? reverse-list
   defgeneric defprototype defmethod new the type?
   catch cond else if label loop throw unless when while error 
   set setter
   push-prompt push-subcont take-subcont push-prompt-subcont
   dlet dnew dref
   defmodule import module
   Arguments Array Date Function Number Object RegExp String
   array array-to-list js-callback js-getter js-global js-invoker list-to-array object log
   @ and or not != % * + - / < <= = > >= in instanceof typeof
   bitand bitor bitxor bitnot bitshiftl bitshiftr bitshiftr0
   print-stacktrace
   ))