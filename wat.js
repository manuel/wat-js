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
    function Begin() {}; function If() {}; function Loop() {};
    function Catch() {}; function Finally() {};
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
    Eval.prototype.wat_combine = function(e, k, f, o) { return evaluate(elt(o, 1), k, f, elt(o, 0)); };
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
    }
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
        pushResume(susp, function(k, f) { return combine(e, null, null, f, NIL); }, cons(this, o));
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
    /* Objects */
    function Nil() {}; var NIL = new Nil();
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) { return cons.car; }
    function cdr(cons) { return cons.cdr; }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function lookup0(e, name) { return e.bindings[name]; }
    function lookup(e, name) { var val = lookup0(e, name); return (val !== undefined) ? val : fail("unbound: " + name); }
    function bind(e, lhs, rhs) { lhs.match(e, rhs); return rhs; }
    function sym_name(sym) { return sym.name; }
    Sym.prototype.match = function(e, rhs) { if (rhs === undefined) fail("trying to match against undefined: " + this.name); 
                                             e.bindings[this.name] = rhs; };
    Cons.prototype.match = function(e, rhs) { car(this).match(e, car(rhs)); cdr(this).match(e, cdr(rhs)); };
    Nil.prototype.match = function(e, rhs) { if (rhs !== NIL) fail("NIL expected"); };
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
        case "[object String]": return new Sym(obj);
        case "[object Array]": return parse_json_array(obj);
        default: return obj; } }
    function parse_json_array(arr) {
        var i = arr.indexOf("");
        if (i === -1) return array_to_list(arr.map(parse_json_value));
        else { var front = arr.slice(0, i);
               return array_to_list(front.map(parse_json_value), arr[i + 1]); } }
    /* JSNI */
    function JSFun(jsfun) { this.jsfun = jsfun; }
    JSFun.prototype.wat_combine = function(e, k, f, o) { return this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    function js_op(op) { return jswrap(new Function("a", "b", "return a " + op + " b")); }
    function js_global(name) { return WAT_GLOBAL[name]; }
    /* Core Environment */
    function envbind(e, name, val) { bind(e, new Sym(name), val); }
    function mkenvcore() {
	var e = new Env();
	envbind(e, "wat-define", new Def());
	envbind(e, "wat-make-environment", jswrap(function (parent) { return new Env(parent); }));
	envbind(e, "wat-eval", wrap(new Eval()));
	envbind(e, "wat-cons", jswrap(cons));
	envbind(e, "wat-list*", jswrap(list_star));
	envbind(e, "wat-string", new JSFun(sym_name));
	envbind(e, "wat-symbol-name", jswrap(sym_name));
	envbind(e, "wat-vau1", new Vau());
	envbind(e, "wat-wrap", jswrap(wrap));
	envbind(e, "wat-unwrap", jswrap(unwrap));
	envbind(e, "wat-begin", new Begin());
	envbind(e, "wat-if", new If());
        envbind(e, "wat-loop1", new Loop());
	envbind(e, "wat-throw", jswrap(fail));
        envbind(e, "wat-catch", wrap(new Catch()));
        envbind(e, "wat-finally", new Finally());
        envbind(e, "wat-macro", jswrap(function(expander) { return new Macro(expander); }));
        envbind(e, "wat-push-prompt-proc", wrap(new PushPrompt()));
        envbind(e, "wat-take-subcont-proc", wrap(new TakeSubcont()));
        envbind(e, "wat-push-subcont-proc", wrap(new PushSubcont()));
        envbind(e, "wat-make-dynamic", wrap(new DNew()));
        envbind(e, "wat-push-dynamic-proc", wrap(new DLet()));
        envbind(e, "wat-peek-dynamic", wrap(new DRef()));
        envbind(e, "wat-js-wrap", jswrap(jswrap));
        envbind(e, "wat-js-global", new JSFun(function(sym) { return js_global(sym_name(sym)); }));
        envbind(e, "wat-js-op", new JSFun(function(sym) { return js_op(sym_name(sym)); }));
	return e;
    }
    var envcore = mkenvcore();
    function run(x) { return evaluate(envcore, null, null, x); }
    /* API */
    return { "run": run, "parse": parse_json_value, "Sym": Sym, "array_to_list": array_to_list };
}
WAT_GLOBAL = this;
// Abbreviations:
// apv: applicative combiner (function)
// arg: argument
// cmb: combiner
// cmt: comment
// e: environment
// f: function to be called on top of newly pushed continuation
// ep: environment parameter
// id: identifier
// k: continuation / resumption
// num: number
// o: operand
// opv: operative combiner (fexpr)
// p: parameter
// str: string
// stx: parser syntax
// sym: symbol
// x: expression
// xe: extended environment
// xs: expressions
