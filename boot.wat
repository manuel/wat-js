;; -*- mode: scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename ur-define
(vm-def _define vm-def)

;; Rename bindings that will be used as provided by VM
(_define apply vm-apply)
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
(_define setter vm-setter)
(_define symbol-name vm-symbol-name)
(_define symbol? vm-symbol?)
(_define throw vm-throw)
(_define unwrap vm-unwrap)
(_define wrap vm-wrap)

;; Important utilities
(_define quote (vm-vau (x) ignore x))
(_define list (wrap (vm-vau elts ignore elts)))
(_define the-environment (vm-vau () e e))

;; Macro and vau
(_define make-macro-expander
  (wrap
    (vm-vau (expander) ignore
      (vm-vau operands env
        (eval (eval (cons expander operands) (make-environment)) env)))))

(_define _vau
  (make-macro-expander
    (vm-vau (params env-param . body) ignore
      (list vm-vau params env-param (list* begin body)))))

(_define macro
  (make-macro-expander
    (_vau (params . body) ignore
      (list make-macro-expander (list* _vau params ignore body)))))

;; Ur-lambda
(_define _lambda
  (macro (params . body)
    (list wrap (list* _vau params ignore body))))

;; Wrap incomplete VM forms
(_define loop
  (macro body
    (list vm-loop (list* begin body))))

(_define catch
  (_vau (protected handler) e
    (eval (list vm-catch protected (eval handler e)) e)))

(_define push-prompt
  (_vau (prompt . body) e
    (eval (list vm-push-prompt (eval prompt e) (list* begin body)) e)))

(_define take-subcont
  (macro (prompt k . body)
    (list vm-take-subcont prompt (list* _lambda (list k) body))))

(_define push-subcont
  (macro (k . body)
    (list vm-push-subcont k (list* _lambda () body))))

;; List utilities
(_define compose (_lambda (f g) (_lambda (arg) (f (g arg)))))

(_define car (_lambda ((x . ignore)) x))
(_define cdr (_lambda ((ignore . x)) x))
(_define caar (compose car car))
(_define cadr (compose car cdr))
(_define cdar (compose cdr car))
(_define cddr (compose cdr cdr))

;; Important macros and functions
(_define define-macro
  (macro ((name . params) . body)
    (list _define name (list* macro params body))))

(_define map-list
  (_lambda (f lst)
    (if (nil? lst)
      ()
      (cons (f (car lst)) (map-list f (cdr lst))))))

(define-macro (let bindings . body)
  (cons
   (list* _lambda (map-list car bindings) body)
   (map-list cadr bindings)))

(define-macro (let* bindings . body)
  (if (nil? bindings)
    (list* let () body)
    (list let (list (car bindings)) (list* let* (cdr bindings) body))))

(_define lambda
  (_vau (params . body) e
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
      (eval (list* _lambda untyped-names (list* begin type-checks) body) e))))

(define-macro (define lhs . rhs)
  (if (cons? lhs)
    (list _define (car lhs) (list* lambda (cdr lhs) rhs))
    (list _define lhs (car rhs))))

;; Simple control
(define-macro (&& a b) (list if a b false))

(define-macro (|| a b) (list if a true b))

(define (call-with-escape fun)
  (let ((fresh (list null)))
    (catch (fun (_lambda opt-arg (throw (list fresh opt-arg))))
      (_lambda (exc)
        (if (&& (cons? exc) (=== fresh (car exc)))
            (let ((opt-arg (cadr exc)))
              (if (cons? opt-arg) (car opt-arg) undefined))
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
  (list if test (list* begin body) null))

(define-macro (unless test . body)
  (list* when (list ! test) body))

(define-macro (set! (getter . args) new-val)
  (list* (list setter getter) new-val args))

;; Delimited dynamic binding

;; Evaluate right hand sides before binding all dynamic variables at once.
(define dlet
  (_vau (bindings . body) e
     (define (process-bindings bs)
       (if (nil? bs)
           (list* begin body)
           (let* ((((name expr) . rest-bs) bs)
                  (value (eval expr e)))
             (list vm-dlet name value (process-bindings rest-bs)))))
     (eval (process-bindings bindings) e)))

;; Prototypes
(define define-prototype
  (_vau (name super prop-names) e
    (let ((p (apply vm-js-make-prototype (list* (symbol-name name) (map-list symbol-name prop-names)))))
      (set! (.prototype (.constructor p)) (new (eval super e)))
      (eval (list _define name p) e))))

(define (put-method ctor name js-fun)
  (set! ((js-getter name) (.prototype ctor)) js-fun))

(define-macro (define-method (name (self ctor) . args) . body)
  (list put-method ctor (symbol-name name)
        (list vm-js-function (list* lambda (list* self args) body))))

(define-macro (define-generic (name . ignore))
  (list _define name (vm-js-invoker (symbol-name name))))

;; Modules
(define provide
  (_vau (symbols . body) env
    (eval (list _define symbols
                (list let ()
                      (list* begin body)
                      (list* list symbols)))
          env)))

(define module
  (_vau (exports . body) e
    (let ((env (make-environment e)))
      (eval (list* provide exports body) env)
      env)))

(define define-module
  (_vau (name exports . body) e
    (eval (list _define name (list* module exports body)) e)))

(define import
  (_vau (module imports) e
    (let* ((m (eval module e))
           (values (map-list (_lambda (import) (eval import m)) imports)))
      (eval (list _define imports (list* list values)) e))))

;; JavaScript

(define-macro (define-js-unop op)
  (list _define op (list vm-js-unop (symbol-name op))))

(define-macro (define-js-binop op)
  (list _define op (list vm-js-binop (symbol-name op))))

(define-js-unop !)
(define-js-unop typeof)
(define-js-unop ~)

(define-js-binop !=)
(define-js-binop !==)
(define-js-binop %)
(define-js-binop &)
(define-js-binop *)
(define-js-binop +)
(define-js-binop -)
(define-js-binop /)
(define-js-binop <)
(define-js-binop <<)
(define-js-binop <=)
(define-js-binop ==)
(define-js-binop ===)
(define-js-binop >)
(define-js-binop >>)
(define-js-binop >>>)
(define-js-binop ^)
(define-js-binop in)
(define-js-binop instanceof)
(define-js-binop |)

(define object
  (_vau pairs e
    (let ((obj (vm-js-make-object)))
      (map-list (_lambda (pair)
                  (let ((name (eval (car pair) e))
                        (value (eval (cadr pair) e)))
                    (set! ((js-getter name) obj) value)))
                pairs)
      obj)))

(define (array . args) (list-to-array args))

(define (@ object key)
  ((js-getter key) object))

(set! (setter @) (lambda (new-val object key)
                   (set! ((js-getter key) object) new-val)))

(define (cat . objects)
  (#join (list-to-array objects) ""))

(define (log . objects)
  (apply #log (list* $console objects)))

(define (js-callback fun)
  (vm-js-function (_lambda args (push-prompt vm-root-prompt (apply fun args)))))

(define-macro (type? obj type)
  (list vm-type? obj type (symbol-name type)))

(define-macro (the type obj)
  (list if (list type? obj type) obj (list error (list cat obj " is not a: " type))))

(define Arguments $Arguments)
(define Array $Array)
(define Date $Date)
(define Function $Function)
(define Number $Number)
(define Object $Object)
(define RegExp $RegExp)
(define String $String)

;; Final events

(define (user-break err)
  (define (print-frame k)
    (log (#toString (.dbg k)) (.e k))
    (if (.next k)
        (print-frame (.next k))
        null))
  (take-subcont vm-root-prompt k
    (print-frame k)
    (push-prompt vm-root-prompt
      (push-subcont k
        (throw err)))))

(define let-redirect
  (_vau (exp bindings . body) env
    (eval (list* (eval (list* _lambda (map-list car bindings) body)
                       (eval exp
                             env))
                 (map-list cadr bindings))
          env)))

(define bindings->environment
  (_vau bindings denv
    (eval (list let-redirect
                (make-environment)
                bindings
                (list the-environment))
          denv)))

(define slurp-environment
  (_vau bindings e
    (eval (list* bindings->environment (map-list (lambda (b) (list b b)) bindings)) e)))

;; Export bindings to userland

;; User environment is subenvironment of environment containing exported bindings
;; so exported bindings cannot be modified

(make-environment 
  (slurp-environment 
   _define _lambda _vau apply eval make-environment the-environment unwrap wrap
   begin define define-macro lambda let let* quote symbol-name symbol?
   caar cadr car cdar cddr cdr cons cons? list list* map-list nil?
   define-generic define-prototype define-method new the type?
   set! setter
   dlet dnew dref
   catch if label loop throw unless when while error 
   push-prompt push-subcont take-subcont
   define-module import module
   Arguments Array Date Function Number Object RegExp String
   array array-to-list js-callback js-getter js-global js-invoker list-to-array object 
   ! != !== % &  * + - / < << <= == === > >> >>> ~ ^ in instanceof typeof
   @ && ||
   cat log
   ))