// -*- fill-column: 120 -*-

// This is an interpreter for a language with fexpr-based syntax and first-class lexical environments based on the vau
// calculus, and higher-order control flow with tail-call elimination and first-class continuations.
//
// The language supports the following expressions:
//
//    `<x>` --- Variable reference: returns the value of the binding it names in the current environment.
// 
//    `(<opr> ...)` --- Operator application: evaluates the operator expression and returns the value of passing the
//    whole form unevaluated to the operator.
//
//    `(def <name> <value>)` --- Evaluates the value expression and binds the name to it in the current environment.
//
//    `(vau <param> <eparam> <body>*)` --- Constructs a compound operator (called "fun" in the code), that remembers the
//    current lexical environment, with the given parameter (which receives the whole form the operator appears in), an
//    environment parameter (which receives the lexical environment the operator is applied in), and a body expression.
//    `_` may be used as parameter or environment parameter to ignore that parameter.
//
//    `(<fun> ...)` --- Compound operator application, i.e. an operator application where the evaluated operator is a
//    compound operator created by `vau`: extends the environment it was created in with bindings for the whole form and
//    the environment it is called in (unless they're ignored), and evaluates the compound operator's body expression in
//    this extended environment, returning the result.
//
//    `(ccc <opr>*)` --- Evaluates the operator expression which must evaluate to an operator, and calls it with the
//    current continuation as argument.
//
//    `(<continuation> <value>)` --- A continuation invocation, i.e. an operator application where the evaluated
//    operator is a continuation: evaluates the value expression, and passes it the continuation, aborting the current
//    computation.
//
//    `(eval <expr>* <env>)` --- Usual doubly-evaluating `eval`: evaluates the expression and environment forms in the
//    current environment, and then evaluates the result of evaluating the expression form in the environment that's the
//    result of evaluating the environment form.
//
// Expressions marked with an asterisk (*) are evaluated or called in tail position when the enclosing form is evaluated
// in tail position, as in Scheme.

var wat = (function() {

    /***** Evaluation *****/

    function evaluate(form, e) {
	var res = perform(form, new KDone(), e); while(typeof(res) === "function") res = res(); return res }

    function perform(form, k, e) { return form.wat_perform ? form.wat_perform(form, k, e) : form }
    Sym.prototype.wat_perform = function(sym, k, e) { return go(k, e, lookup(e, sym)) }
    Xons.prototype.wat_perform = function(xons, k, e) { return perform(car(xons), new KApp(k, xons), e) }

    function Fun(param, eparam, body, e) { this.param = param; this.eparam = eparam; this.body = body; this.e = e }
    function Def() {}; function CCC() {}; function Vau() {}; function Eval() {};
    function operate(opr, opd, k, e) { return opr.wat_operate(opr, opd, k, e) }
    Fun.prototype.wat_operate = function(opr, opd, k, e) {
	var xe = extend(opr.e); bind(xe, opr.param, opd); bind(xe, opr.eparam, e); return perform(opr.body, k, xe) }
    Def.prototype.wat_operate = function(opr, opd, k, e) { return perform(elt(opd, 2), new KDef(k, elt(opd, 1)), e) }
    CCC.prototype.wat_operate = function(opr, opd, k, e) { return perform(elt(opd, 1), new KApp(k, k), e) }
    Vau.prototype.wat_operate = function(opr, opd, k, e) { return go(k, e, new Fun(elt(opd, 1), elt(opd, 2), elt(opd, 3), e)) }
    Eval.prototype.wat_operate = function(opr, opd, k, e) { return perform(elt(opd, 1), new KEval1(k, elt(opd, 2)), e) }

    function KDone() {}
    function KApp(next, opd) { this.next = next; this.opd = opd }
    function KDef(next, name) { this.next = next; this.name = name }
    function KEval1(next, eform) { this.next = next; this.eform = eform }
    function KEval2(next, form) { this.next = next; this.form = form }
    function KJump(k) { this.k = k }
    function go(k, e, val) { return k.wat_go(k, e, val) }
    KDone.prototype.wat_go = function(k, e, val) { return val }
    KApp.prototype.wat_go = function(k, e, opr) { return function() { return operate(opr, k.opd, k.next, e) } }
    KDef.prototype.wat_go = function(k, e, val) { return function() { bind(e, k.name, val); return go(k.next, e, VOID) } }
    KEval1.prototype.wat_go = function(k, e, form) { return function() { return perform(k.eform, new KEval2(k.next, form), e) } }
    KEval2.prototype.wat_go = function(k, e, newe) { return function() { return perform(k.form, k.next, newe) } }
    KJump.prototype.wat_go = function(k, e, val) { return function() { return go(k.k, e, val) } }

    function koperate(opr, opd, k, e) { return perform(elt(opd, 1), new KJump(opr), e) }
    KDone.prototype.wat_operate = koperate; KApp.prototype.wat_operate = koperate; KDef.prototype.wat_operate = koperate
    KEval1.prototype.wat_operate = koperate; KEval2.prototype.wat_operate = koperate; KJump.prototype.wat_operate = koperate

    // `evaluate` is the main interface for evaluation: it takes a form and computes its value in a specified
    // environment.  Every evaluation step returns either a function or a value.  If it returns a function, this
    // function is again applied as a new evaluation step.  This trampoline enables tail-call elimination and
    // first-class continuations.  `evaluate` ends computation with the `KDone` continuation (discussed below), which is
    // appended to the continuation chain and halts evaluation by returning the value with which it is invoked.
    //
    // `perform` is the main function for evaluating a single form: it takes a form, a continuation, and an environment
    // and invokes the continuation with the value computed from the form in the environment.  Symbols and xonses
    // (compound forms, discussed below) have special evaluation behavior (symbols evaluate to the value of the binding
    // they name, xonses evaluate to the result of the combination of their operator (`car`) with themselves), all other
    // forms (e.g. literals) evaluate to themselves.
    // 
    // `operate` takes an operator, and operand (always the whole form the operator appears in), a continuation, and an
    // environment.  It passes the result of combining the operator with the operand in the environment to the
    // continuation (combining is the vau calculus term for applying an operator).  Every operator defines a
    // `wat_operate` function with its specific behavior.
    // 
    // Continuations (or rather, continuation frames) start with the letter "K".  `go' jumps to a continuation with a
    // specified value and environment.  This is called invoking a continuation.  Every continuation has a `wat_go`
    // method with its specific invocation behavior.  `wat_go` should return a function if there are more computation
    // steps to be performed, or a value, which indicates the end of evaluation.
    //
    //   `KDone` is always appended to the continuation chain as the final step.  It halts evaluation with the value it
    //   receives.
    //
    //   `KApp` is the second part of evaluating an operator combination: it receives the operator, and applies it to
    //   its operand (which is the whole compound form in which the operator appears as `car`), returning the result.
    // 
    //   `KDef is the second part of evaluating a name definition: it receives the value, and binds the name to that
    //   value in the current environment.  It returns `VOID`.
    //
    //   `KEval1` and `KEval2` are the second and third step of evaluating an arbitrary form in an arbitrary environment
    //   i.e. `(eval form eform)`.  First, the argument for the form to be evaluated, `form`, is evaluated, and passed
    //   to `KEval1` which temporarily remembers the argument for the environment to be evaluated in, `eform`.  When
    //   `KEval1` receives the value of `form`, it evaluates the argument for the environment `eform`, and passes that
    //   to `KEval2` which temporarily remembers the value of `form`.  Then, `KEval2` receives the value of `eform`, and
    //   we can now evaluate the value of `form` in the environment that's the value of `eform`, returning the result.
    //   
    //   `KJump` is used when a user invokes a continuation by applying a continuation as an operator.  It receives a
    //   value and passes that to a specified continuation, aborting the current computation.
    //
    // `koperate` makes continuations usable as operators, allowing callers of `ccc` to jump to the continuation.
    //
    // `JSFunUnary` is a bridge for using ordinary JavaScript functions as operators that require a single evaluated
    // argument.  `KJSAppUnary` is called with an evaluated argument, and passes the current environment and the
    // argument to a JavaScript function.

    function JSFunNullary(jsfun) { this.jsfun = jsfun }
    JSFunNullary.prototype.wat_operate = function(opr, opd, k, e) { return go(k.next, e, opr.jsfun.call(null, e)) }
    function JSFunUnary(jsfun) { this.jsfun = jsfun }
    JSFunUnary.prototype.wat_operate = function(opr, opd, k, e) { return perform(elt(opd, 1), new KJSAppUnary(k, opr.jsfun), e) }
    function KJSAppUnary(next, jsfun) { this.next = next; this.jsfun = jsfun }
    KJSAppUnary.prototype.wat_go = function(k, e, val) { return function() { return go(k.next, e, k.jsfun.call(null, e, val)) } }
    KJSAppUnary.prototype.wat_operate = koperate

    /***** Data *****/

    function Sym(name) { this.name = name }

    function Xons(entries) { this.entries = entries }
    function car(xons) { return xons.entries.car }; function cdr(xons) { return xons.entries.cdr }
    function cons(car, cdr) { return new Xons({ car: car, cdr: cdr }) }
    function elt(xons, i) { return (i === 0) ? car(xons) : elt(cdr(xons), i - 1) }

    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null) }
    function lookup(e, sym) { return e.bindings[sym.name] }
    function bind(e, sym, val) { if (sym !== IGN) e.bindings[sym.name] = val }

    function Str(jsstr) { this.jsstr = jsstr }
    function Num(jsnum) { this.jsnum = jsnum }

    function Void() {}; function Ign() {}; function Nil() {}
    var VOID = new Void(); var IGN = new Ign(); var NIL = new Nil()

    function Tag() { this.e = new Env() }
    function Tagged(tag, val) { this.wat_tag = tag; this.val = val }
    
    Tag.prototype.wat_tag = new Tag()
    Sym.prototype.wat_tag = new Tag()
    Xons.prototype.wat_tag = new Tag()
    Env.prototype.wat_tag = new Tag()
    Str.prototype.wat_tag = new Tag()
    Num.prototype.wat_tag = new Tag()
    Void.prototype.wat_tag = new Tag()
    Ign.prototype.wat_tag = new Tag()
    Nil.prototype.wat_tag = new Tag()
    Fun.prototype.wat_tag = new Tag()
    Def.prototype.wat_tag = new Tag()
    CCC.prototype.wat_tag = new Tag()
    Vau.prototype.wat_tag = new Tag()
    Eval.prototype.wat_tag = new Tag()
    KDone.prototype.wat_tag = new Tag()
    KApp.prototype.wat_tag = new Tag()
    KDef.prototype.wat_tag = new Tag()
    KEval1.prototype.wat_tag = new Tag()
    KEval2.prototype.wat_tag = new Tag()
    KJump.prototype.wat_tag = new Tag()
    JSFunNullary.prototype.wat_tag = new Tag()
    JSFunUnary.prototype.wat_tag = new Tag()
    KJSAppUnary.prototype.wat_tag = new Tag()

    // Core Environment

    var lib_mkenv = new JSFunUnary(function (e, parent) { return new Env(parent !== VOID ? parent : null) })
    var lib_mktag = new JSFunNullary(function (e) { return new Tag() })
    var lib_tag = new JSFunUnary(function (e, obj) { return obj.wat_tag })
    var lib_tagger = new JSFunUnary(function (e, tag) { return new JSFunUnary(function (e, val) { return new Tagged(tag, val) }) })

    function mkenvcore() {
	var e = new Env()
	bind(e, new Sym("def"), new Def())
	bind(e, new Sym("ccc"), new CCC())
	bind(e, new Sym("vau"), new Vau())
	bind(e, new Sym("eval"), new Eval())
	bind(e, new Sym("mkenv"), lib_mkenv)
	bind(e, new Sym("mktag"), lib_mktag)
	bind(e, new Sym("tag"), lib_tag)
	bind(e, new Sym("tagger"), lib_tagger)
	return e
    }

    // API

    return {
	"eval": evaluate, "mkenvcore": mkenvcore,
	"car": car, "cdr": cdr, "cons": cons,
	"Sym": Sym, "Xons": Xons, "Str": Str, "Num": Num,
	"VOID": VOID, "IGN": IGN, "NIL": NIL,
    }

}())
