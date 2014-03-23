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
(_define symbol-name vm-symbol-name)
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

(_define dlet
  (_vau (dv val . body) e
    (eval (list vm-dlet (eval dv e) (eval val e) (list* begin body)) e)))

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

(define-macro (define lhs . rhs)
  (if (cons? lhs)
    (list _define (car lhs) (list* _lambda (cdr lhs) rhs))
    (list _define lhs (car rhs))))

(define (map-list f lst)
  (if (nil? lst)
   ()
   (cons (f (car lst)) (map-list f (cdr lst)))))

(define-macro (let bindings . body)
  (cons
    (list* _lambda (map-list car bindings) body)
    (map-list cadr bindings)))

(define-macro (let* bindings . body)
  (if (nil? bindings)
    (list* let () body)
    (list let (list (car bindings)) (list* let* (cdr bindings) body))))

;; Use ur-lambda as lambda for now but this will probably be
;; replaced with a typed lambda soon
(define lambda _lambda)

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

;; Prototypes
(define-macro (define-prototype name prop-names)
  (list _define name
        (list* vm-js-make-prototype (symbol-name name)
               (map-list symbol-name prop-names))))

(define (put-method ctor name js-fun)
  ((vm-js-setter name) (.prototype ctor) js-fun))

(define-macro (define-method (name (self ctor) . args) . body)
  (list put-method ctor (symbol-name name)
        (list vm-js-function (list* _lambda (list* self args) body))))

(define-macro (define-generic (name . ignore))
  (list _define name (_lambda args
                       (apply (vm-js-invoker (symbol-name name)) args))))

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
                    ((vm-js-setter name) obj value)))
                pairs)
      obj)))

(define (array . args) (list-to-array args))

(define (@ object key)
  ((vm-js-getter key) object))

(define (js-callback fun)
  (vm-js-function (_lambda args (push-prompt vm-root-prompt (apply fun args)))))

(define-macro (the type obj)
  (list vm-type-check (symbol-name type) type obj))

(define Arguments $Arguments)
(define Array $Array)
(define Date $Date)
(define Function $Function)
(define Number $Number)
(define Object $Object)
(define RegExp $RegExp)
(define String $String)

(define (cat . objects)
  (#join (list-to-array objects) ""))

(define (log . objects)
  (apply #log (list* $console objects)))

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

(make-environment (slurp-environment list ! != !== % & && * + - / < <<
<= == === > >> >>> @ Arguments Array Date Function Number Object
RegExp String ^ _define _lambda _vau apply array array-to-list begin
caar cadr car cat catch cdar cddr cdr cons cons? define define-generic
define-macro define-method define-module define-prototype dlet dnew
dref error eval the-environment if import in instanceof js-callback
js-getter js-global js-invoker label lambda let let* list list*
list-to-array log loop macro make-environment map-list module new nil?
object provide push-prompt push-subcont quote symbol-name take-subcont
the throw typeof unless unwrap when while wrap || ~))