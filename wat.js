function Wat() {
    /* Continuations */
    function Suspension(prompt, handler) {
        this.prompt = prompt; this.handler = handler; this.resumption = null; }
    function isSuspend(x) { return x instanceof Suspension; }
    function Resumption(fun, next, dbg) {
        this.fun = fun; this.next = next; this.dbg = dbg; }
    function isResume(x) { return x instanceof Resumption; }
    function pushResume(susp, fun, dbg) {
        susp.resumption = new Resumption(fun, susp.resumption, dbg); }
    function resume(resumption, f) {
        return resumption.fun(resumption.next, f); }
    /* Evaluation Core */
    function evaluate(e, k, f, x) {
        if (x && x.wat_eval) return x.wat_eval(e, k, f); else return x; }
    function Sym(name) { this.name = name; }
    Sym.prototype.wat_eval = function(e, k, f) {
        return lookup(e, this.name); };
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    Cons.prototype.wat_eval = function(e, k, f) {
        if (isResume(k)) {
            var op = resume(k, f);
        } else {
            var op = evaluate(e, null, null, car(this));
        }
        if (isSuspend(op)) {
            var that = this;
            pushResume(op, function(k, f) { return that.wat_eval(e, k, f); }, that);
            return op;
        }
        if (isMacro(op)) {
            return macroCombine(e, null, null, op, this);
        } else {
            return combine(e, null, null, op, cdr(this));
        }
    };
    function macroCombine(e, k, f, macro, form) {
        if (isResume(k)) {
            var expanded = resume(k, f);
        } else {
            var expanded = combine(e, k, f, macro.expander, cdr(form));
        }
        if (isSuspend(expanded)) {
            pushResume(expanded, function(k, f) { return macroCombine(e, k, f, macro, form); });
            return expanded;
        }
        form.car = expanded.car;
        form.cdr = expanded.cdr;
        return evaluate(e, k, f, form);
    }
    function Macro(expander) { this.expander = expander; }
    function isMacro(x) { return x instanceof Macro; }
    /* Operative & Applicative Combiners */
    function combine(e, k, f, cmb, o) {
        if (cmb && cmb.wat_combine) return cmb.wat_combine(e, k, f, o); else fail("not a combiner"); }
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }; function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; }
    Opv.prototype.wat_combine = function(e, k, f, o) {
	var xe = new Env(this.e); bind(xe, this.p, o); bind(xe, this.ep, e);
        return evaluate(xe, k, f, this.x);
    };
    Apv.prototype.wat_combine = function(e, k, f, o) {
        if (isResume(k)) {
            var args = resume(k, f);
        } else {
            var args = evalArgs(e, null, null, o, NIL);
        }
        if (isSuspend(args)) {
            var that = this;
            pushResume(args, function(k, f) { return that.wat_combine(e, k, f, o); }, cons(this, o));
            return args;
        }
        return this.cmb.wat_combine(e, null, null, args);
    };
    function evalArgs(e, k, f, todo, done) {
	if (todo === NIL) { return reverse_list(done); }
        if (isResume(k)) {
            var arg = resume(k, f);
        } else {
            var arg = evaluate(e, null, null, car(todo));
        }
        if (isSuspend(arg)) {
            pushResume(arg, function(k, f) { return evalArgs(e, k, f, todo, done); });
            return arg;
        }
        return evalArgs(e, null, null, cdr(todo), cons(arg, done));
    };
    /* Built-in Combiners */
    function Vau() {}; function Def() {}; function Eval() {};
    Vau.prototype.wat_combine = function(e, k, f, o) { return new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e); };
    Def.prototype.wat_combine = function(e, k, f, o) {
        if (isResume(k)) {
            var val = resume(k, f);
        } else {
            var val = evaluate(e, null, null, elt(o, 1));
        }
        if (isSuspend(val)) {
            pushResume(val, function(k, f) { return Def.prototype.wat_combine(e, k, f, o); }, cons(this,o));
            return val;
        }
        return bind(e, elt(o, 0), val);
    };
    Eval.prototype.wat_combine = function(e, k, f, o) { return evaluate(elt(o, 1), k, f, elt(o, 0)); };
    /* First-order Control */
    function Begin() {}; function If() {}; function Loop() {}; function Catch() {}; function Finally() {};
    Begin.prototype.wat_combine = function(e, k, f, o) { if (o === NIL) fail("empty wat-begin"); else return begin(e, k, f, o); };
    function begin(e, k, f, xs) {
        if (isResume(k)) {
            var res = resume(k, f);
        } else {
            var res = evaluate(e, null, null, car(xs));
        }
        if (isSuspend(res)) {
            pushResume(res, function(k, f) { return begin(e, k, f, xs); });
            return res;
        }
        var kdr = cdr(xs);
        if (kdr === NIL) return res; else return begin(e, null, null, kdr);
    }
    If.prototype.wat_combine = function(e, k, f, o) {
        if (isResume(k)) {
            var test = resume(k, f);
        } else {
            var test = evaluate(e, null, null, elt(o, 0));
        }
        if (isSuspend(test)) {
            pushResume(test, function(k, f) { return If.prototype.wat_combine(e, k, f, o); }, cons(this,o));
            return test;
        }
        return evaluate(e, null, null, test === false ? elt(o, 2) : elt(o, 1));
    };
    Loop.prototype.wat_combine = function(e, k, f, o) {
        var first = true; // only resume once
        while (true) {
            if (first && isResume(k)) {
                var res = resume(k, f);
            } else {
                var res = evaluate(e, null, null, elt(o, 0));
            }
            first = false;
            if (isSuspend(res)) {
                pushResume(res, function(k, f) { return Loop.prototype.wat_combine(e, k, f, o); }, cons(this,o));
                return res;
            }
        }
    };
    Catch.prototype.wat_combine = function(e, k, f, o) {
        var th = elt(o, 0);
        var handler = elt(o, 1);
        try {
            if (isResume(k)) {
                var res = resume(k, f);
            } else {
                var res = combine(e, null, null, th, NIL);
            }
        } catch(exc) {
            var res = combine(e, null, null, handler, list(exc));
        }
        if (isSuspend(res)) {
            pushResume(res, function(k, f) { return Catch.prototype.wat_combine(e, k, f, o); }, cons(this,o));
            return res;
        } else {
            return res;
        }
    };
    Finally.prototype.wat_combine = function(e, k, f, o) {
        var prot = elt(o, 0);
        var cleanup = elt(o, 1);
        try {
            if (isResume(k)) {
                var res = resume(k, f);
            } else {
                var res = evaluate(e, null, null, prot);
            }
            if (isSuspend(res)) {
                pushResume(res, function(k, f) { return Finally.prototype.wat_combine(e, k, f, o); }, cons(this,o));
            }
        } finally {
            if (isSuspend(res)) {
                return res;
            } else {
                return doCleanup(e, null, null, cleanup, res);
            }
        }
    };
    function doCleanup(e, k, f, cleanup, res) {
        if (isResume(k)) {
            var fres = resume(k, f);
        } else {
            var fres = evaluate(e, null, null, cleanup);
        }
        if (isSuspend(fres)) {
            pushResume(fres, function(k, f) { return doCleanup(e, k, f, cleanup, res); });
            return fres;
        } else {
            return res;
        }
    }
    /* Delimited Control */
    function PushPrompt() {}; function TakeSubcont() {}; function PushSubcont() {};
    PushPrompt.prototype.wat_combine = function(e, k, f, o) {
        var prompt = elt(o, 0);
        var th = elt(o, 1);
        if (isResume(k)) {
            var res = resume(k, f);
        } else {
            var res = combine(e, null, null, th, NIL);
        }
        if (isSuspend(res)) {
            if (res.prompt === prompt) {
                var resumption = res.resumption;
                var handler = res.handler;
                return combine(e, null, null, handler, cons(resumption, NIL));
            } else {
                pushResume(res, function(k, f) { return PushPrompt.prototype.wat_combine(e, k, f, o); }, cons(this,o));
                return res;
            }
        } else {
            return res;
        }
    };
    TakeSubcont.prototype.wat_combine = function(e, k, f, o) {
        var prompt = elt(o, 0);
        var handler = elt(o, 1);
        var susp = new Suspension(prompt, handler);
        pushResume(susp, function(k, thef) { return combine(e, null, null, thef, NIL); }, cons(this, o));
        return susp;
    };
    PushSubcont.prototype.wat_combine = function(e, k, f, o) {
        var thek = elt(o, 0);
        var thef = elt(o, 1);
        if (isResume(k)) {
            var res = resume(k, f);
        } else {
            var res = resume(thek, thef);
        }
        if (isSuspend(res)) {
            pushResume(res, function(k, f) { return PushSubcont.prototype.wat_combine(e, k, f, o); }, cons(this,o));
            return res;
        } else {
            return res;
        }
    };
    /* Dynamic Variables */
    function DV(val) { this.val = val; }
    function DNew() {}; function DLet() {}; function DRef() {};
    DNew.prototype.wat_combine = function(e, k, f, o) {
        return new DV(elt(o, 0));
    }
    DLet.prototype.wat_combine = function(e, k, f, o) {
        var dv = elt(o, 0);
        var val = elt(o, 1);
        var th = elt(o, 2);
        var oldVal = dv.val;
        dv.val = val;
        try {
            if (isResume(k)) {
                var res = resume(k, f);
            } else {
                var res = combine(e, null, null, th, NIL);
            }
            if (isSuspend(res)) {
                pushResume(res, function(k, f) { return DLet.prototype.wat_combine(e, k, f, o); }, cons(this,o));
                return res;
            } else {
                return res;
            }
        } finally {
            dv.val = oldVal;
        }
    }
    DRef.prototype.wat_combine = function(e, k, f, o) {
        return elt(o, 0).val;
    };
    /* Objects */
    function Nil() {}; var NIL = new Nil();
    function Ign() {}; var IGN = new Ign();
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) { return cons.car; }
    function cdr(cons) { return cons.cdr; }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function lookup0(e, name) { return e.bindings[name]; }
    function lookup(e, name) {
        var val = lookup0(e, name);
        return (typeof(val) !== "undefined") ? val : fail("unbound: " + name); }
    function bind(e, lhs, rhs) { lhs.match(e, rhs); return rhs; }
    function sym_name(sym) { return sym.name; }
    Sym.prototype.match = function(e, rhs) {
        if (typeof(e) === "undefined") fail("undefined argument: " + this.name);
        return e.bindings[this.name] = rhs; }
    Cons.prototype.match = function(e, rhs) { car(this).match(e, car(rhs)); cdr(this).match(e, cdr(rhs)); };
    Nil.prototype.match = function(e, rhs) { if (rhs !== NIL) fail("NIL expected"); };
    Ign.prototype.match = function(e, rhs) {};
    /* Utilities */
    function fail(err) { throw err; }
    function list() {
        return array_to_list(Array.prototype.slice.call(arguments)); }
    function list_star() {
        var len = arguments.length;
	var c = len >= 1 ? arguments[len-1] : NIL; for (var i = len-1; i > 0; i--) c = cons(arguments[i - 1], c); return c; }
    function array_to_list(array, end) {
	var c = end ? end : NIL; for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c); return c; }
    function list_to_array(c) {
	var res = []; while(c !== NIL) { res.push(car(c)); c = cdr(c); } return res; }
    function reverse_list(list) {
	var res = NIL; while(list !== NIL) { res = cons(car(list), res); list = cdr(list); } return res; }
    /* Parser */
    function parse_json_value(obj) {
        switch(Object.prototype.toString.call(obj)) {
        case "[object String]": return obj === "#ignore" ? IGN : new Sym(obj);
        case "[object Array]": return parse_json_array(obj);
        default: return obj; } }
    function parse_json_array(arr) {
        var i = arr.indexOf("#rest");
        if (i === -1) return array_to_list(arr.map(parse_json_value));
        else { var front = arr.slice(0, i);
               return array_to_list(front.map(parse_json_value), parse_json_value(arr[i + 1])); } }
    /* JSNI */
    function JSFun(jsfun) { if (Object.prototype.toString.call(jsfun) !== "[object Function]") fail("no fun");
                            this.jsfun = jsfun; }
    JSFun.prototype.wat_combine = function(e, k, f, o) { return this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    function js_op(op) { return jswrap(new Function("a", "b", "return a " + op + " b")); }
    function js_global(name) { return eval(name); }
    function js_invoke(obj, method_name) {
        return obj[sym_name(method_name)].apply(obj, Array.prototype.slice.call(arguments, 2)); }
    /* Microcode */
    var microcode =
        ["wat-begin",

         // Core

         // Fexprs and Macros
         ["wat-def", "wat-vau", new Vau()],
         ["wat-def", "wat-wrap", jswrap(wrap)],
         ["wat-def", "wat-macro", jswrap(function(expander) { return new Macro(expander); })],
         // Forms
         ["wat-def", "wat-cons", jswrap(cons)],
         ["wat-def", "wat-cons?", jswrap(function(obj) { return obj instanceof Cons; })],
         ["wat-def", "wat-nil?", jswrap(function(obj) { return obj === NIL; })],
         // First-order Control
         ["wat-def", "wat-if", new If()],
         ["wat-def", "wat-loop", new Loop()],
         ["wat-def", "wat-throw", jswrap(fail)],
         ["wat-def", "wat-catch", wrap(new Catch())],
         ["wat-def", "wat-finally", new Finally()],
         // Delimited Control
         ["wat-def", "wat-push-prompt", wrap(new PushPrompt())],
         ["wat-def", "wat-take-subcont", wrap(new TakeSubcont())],
         ["wat-def", "wat-push-subcont", wrap(new PushSubcont())],
         // Dynamically-scoped Variables
         ["wat-def", "wat-dnew", wrap(new DNew())],
         ["wat-def", "wat-dlet", wrap(new DLet())],
         ["wat-def", "wat-dref", wrap(new DRef())],
         // JS Interface
         ["wat-def", "wat-js-wrap", jswrap(jswrap)],
         ["wat-def", "wat-js-global", jswrap(function(sym) { return js_global(sym_name(sym)); })],
         ["wat-def", "wat-js-op", new JSFun(function(sym) { return js_op(sym_name(sym)); })],
         ["wat-def", "wat-js-invoke", jswrap(js_invoke)],
         // Optimization
         ["wat-def", "wat-list*", jswrap(list_star)],
         
         // Primitives

         ["wat-def", "def", "wat-def"],
         ["def", "begin", "wat-begin"],
         ["def", "cons", "wat-cons"],
         ["def", "cons?", "wat-cons?"],
         ["def", "if", "wat-if"],
         ["def", "list*", "wat-list*"],
         ["def", "nil?", "wat-nil?"],

         ["def", "quote", ["wat-vau", ["x"], "#ignore", "x"]],
         ["def", "list", ["wat-wrap", ["wat-vau", "arglist", "#ignore", "arglist"]]],

         ["def", "vau",
          ["wat-macro",
           ["wat-vau", ["params", "env-param", "#rest", "body"], "#ignore",
            ["list", "wat-vau", "params", "env-param", ["cons", "begin", "body"]]]]],

         ["def", "macro",
          ["wat-macro",
           ["wat-vau", ["params", "#rest", "body"], "#ignore",
            ["list", "wat-macro", ["list*", "vau", "params", "#ignore", "body"]]]]],

         ["def", "define-macro",
          ["macro", [["name", "#rest", "params"], "#rest", "body"],
           ["list", "def", "name", ["list*", "macro", "params", "body"]]]],

         ["define-macro", ["lambda", "params", "#rest", "body"],
          ["list", "wat-wrap", ["list*", "vau", "params", "#ignore", "body"]]],
         ["define-macro", ["loop", "#rest", "body"],
          ["list", "wat-loop1", ["list*", "begin", "body"]]],

         ["define-macro", ["push-prompt", "prompt", "#rest", "body"],
          ["list", "push-prompt*", "prompt", ["list*", "lambda", [], "body"]]],
         ["define-macro", ["take-subcont", "prompt", "k", "#rest", "body"],
          ["list", "take-subcont*", "prompt", ["list*", "lambda", ["list", "k"], "body"]]],
         ["define-macro", ["push-subcont", "k", "#rest", "body"],
          ["list", "push-subcont*", "k", ["list*", "lambda", [], "body"]]],

         ["def", "compose", ["lambda", ["f", "g"], ["lambda", ["arg"], ["g", ["f", "arg"]]]]],

         ["def", "car", ["lambda", [["x", "#rest", "#ignore"]], "x"]],
         ["def", "cdr", ["lambda", [["#ignore", "#rest", "x"]], "x"]],
         ["def", "caar", ["compose", "car", "car"]],
         ["def", "cadr", ["compose", "car", "cdr"]],
         ["def", "cdar", ["compose", "cdr", "car"]],
         ["def", "cddr", ["compose", "cdr", "cdr"]],

         ["define-macro", ["define", "lhs", "#rest", "rhs"],
          ["if", ["cons?", "lhs"],
           ["list", "def", ["car", "lhs"], ["list*", "lambda", ["cdr", "lhs"], "rhs"]],
           ["list", "def", "lhs", ["car", "rhs"]]]],

         ["define", ["map-list", "f", "lst"],
           ["if", ["nil?", "lst"],
            [],
            ["cons", ["f", ["car", "lst"]], ["map-list", "f", ["cdr", "lst"]]]]],

         ["define-macro", ["let", "bindings", "#rest", "body"],
          ["list*",
           ["list*", "lambda", ["map-list", "car", "bindings"], "body"],
           ["map-list", "cadr", "bindings"]]],

         ["define-macro", ["define-js-op", "op"],
          ["list", "define", "op", ["list", "wat-js-op", "op"]]],
         ["define-js-op", "*"],
         ["define-js-op", "+"],
         ["define-js-op", "-"],
         ["define-js-op", "/"],
         ["define-js-op", "<"],
         ["define-js-op", "<="],
         ["define-js-op", "=="],
         ["define-js-op", "==="],
         ["define-js-op", ">"],
         ["define-js-op", ">="],

         ["define-macro", ["not", "x"],
          ["list", "if", "x", false, true]]
         
        ];
    /* Init */
    var environment = new Env();
    bind(environment, new Sym("wat-def"), new Def());
    bind(environment, new Sym("wat-begin"), new Begin());
    run(microcode);
    /* API */
    function run(x) { return evaluate(environment, null, null, parse_json_value(x)); }
    return { "run": run };
}
