(function(wat) {
wat.VM = function() {
    /* Continuations */
    function Continuation(fun, next, dbg, e) {
        this.fun = fun; this.next = next; this.dbg = dbg; this.e = e; }
    function isContinuation(x) { return x instanceof Continuation; }
    function Capture(prompt, handler) {
        this.prompt = prompt; this.handler = handler; this.k = null; }
    function isCapture(x) { return x instanceof Capture; }
    function captureFrame(capture, fun, dbg, e) {
        capture.k = new Continuation(fun, capture.k, dbg, e); }
    function continueFrame(k, f) {
        return k.fun(k.next, f); }
    /* Evaluation Core */
    function evaluate(e, k, f, x) {
        if (x && x.wat_eval) return x.wat_eval(e, k, f); else return x; }
    function Sym(name) { this.name = name; }
    Sym.prototype.wat_eval = function(e, k, f) { return lookup(e, this.name); };
    Sym.prototype.toString = function() { return this.name; };
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    Cons.prototype.wat_eval = function(e, k, f) {
        if (isContinuation(k)) {
            var op = continueFrame(k, f);
        } else {
            var op = evaluate(e, null, null, car(this));
        }
        if (isCapture(op)) {
            var that = this;
            captureFrame(op, function(k, f) { return that.wat_eval(e, k, f); }, this, e);
            return op;
        }
        return combine(e, null, null, op, cdr(this));
    };
    Cons.prototype.toString = function() { return "(" + cons_to_string(this) + ")" };
    function cons_to_string(cons) {
        if (cons.cdr !== NIL) {
            return to_string(cons.car) + " " + cons_to_string(cons.cdr);
        } else {
            return to_string(cons.car);
        }
    }
    /* Operative & Applicative Combiners */
    function combine(e, k, f, cmb, o) {
        if (cmb && cmb.wat_combine) return cmb.wat_combine(e, k, f, o);
        else return fail("not a combiner: " + to_string(cmb)); }
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }
    Apv.prototype.toString = function() { return "[Apv " + to_string(this.cmb) + "]"; };
    function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; }
    Opv.prototype.wat_combine = function(e, k, f, o) {
        var xe = make_env(this.e); 
        var pCap = bind(xe, this.p, o);
        if (isCapture(pCap)) return pCap;
        var epCap = bind(xe, this.ep, e);
        if (isCapture(epCap)) return epCap;
        return evaluate(xe, k, f, this.x);
    };
    Opv.prototype.toString = function() {
        return "[Opv " + to_string(this.p) + " " + to_string(this.ep) + " " + to_string(this.x) + "]"; };
    Apv.prototype.wat_combine = function(e, k, f, o) {
        if (isContinuation(k)) {
            var args = continueFrame(k, f);
        } else {
            var args = evalArgs(e, null, null, o, NIL);
        }
        if (isCapture(args)) {
            var that = this;
            captureFrame(args, function(k, f) { return that.wat_combine(e, k, f, o); }, cons(this, o), e);
            return args;
        }
        return this.cmb.wat_combine(e, null, null, args);
    };
    function evalArgs(e, k, f, todo, done) {
        if (todo === NIL) { return reverse_list(done); }
        if (isContinuation(k)) {
            var arg = continueFrame(k, f);
        } else {
            var arg = evaluate(e, null, null, car(todo));
        }
        if (isCapture(arg)) {
            captureFrame(arg, function(k, f) { return evalArgs(e, k, f, todo, done); }, car(todo), e);
            return arg;
        }
        return evalArgs(e, null, null, cdr(todo), cons(arg, done));
    }
    /* Built-in Combiners */
    function __Vau() {}; function Def() {}; function Eval() {}
    __Vau.prototype.toString = function() { return "vau"; };
    Def.prototype.toString = function() { return "def"; };
    Eval.prototype.toString = function() { return "eval"; };
    __Vau.prototype.wat_combine = function(e, k, f, o) {
        return new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e); };
    Def.prototype.wat_combine = function self(e, k, f, o) {
        var lhs = elt(o, 0); if (isCapture(lhs)) return lhs;
        var rhs = elt(o, 1); if (isCapture(rhs)) return rhs;
        if (isContinuation(k)) {
            var val = continueFrame(k, f);
        } else {
            var val = evaluate(e, null, null, rhs);
        }
        if (isCapture(val)) {
            captureFrame(val, function(k, f) { return self(e, k, f, o); }, rhs, e);
            return val;
        }
        return bind(e, lhs, val);
    };
    Eval.prototype.wat_combine = function(e, k, f, o) {
        var x = elt(o, 0); if (isCapture(x)) return x;
        var e = elt(o, 1); if (isCapture(e)) return e;
        return evaluate(e, k, f, x); };
    /* First-order Control */
    function Begin() {}; function If() {}; function __Loop() {}
    function __Catch() {}; function Finally() {}
    Begin.prototype.toString = function() { return "begin"; };
    If.prototype.toString = function() { return "if"; };
    __Loop.prototype.toString = function() { return "loop"; };
    __Catch.prototype.toString = function() { return "catch"; };
    Finally.prototype.toString = function() { return "finally"; };
    Begin.prototype.wat_combine = function(e, k, f, o) {
        if (o === NIL) return null; else return begin(e, k, f, o); };
    function begin(e, k, f, xs) {
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = evaluate(e, null, null, car(xs));
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return begin(e, k, f, xs); }, car(xs), e);
            return res;
        }
        var kdr = cdr(xs);
        if (kdr === NIL) return res; else return begin(e, null, null, kdr);
    }
    If.prototype.wat_combine = function self(e, k, f, o) {
        if (isContinuation(k)) {
            var test = continueFrame(k, f);
        } else {
            var test = evaluate(e, null, null, elt(o, 0));
        }
        if (isCapture(test)) {
            captureFrame(test, function(k, f) { return self(e, k, f, o); }, elt(o, 0), e);
            return test;
        }
        return evaluate(e, null, null, test ? elt(o, 1) : elt(o, 2));
    };
    __Loop.prototype.wat_combine = function self(e, k, f, o) {
        var first = true; // only continue once
        while (true) {
            if (first && isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, elt(o, 0));
            }
            first = false;
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, elt(o, 0), e);
                return res;
            }
        }
    };
    __Catch.prototype.wat_combine = function self(e, k, f, o) {
        var th = elt(o, 0);
        var handler = elt(o, 1);
        try {
            if (isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = combine(e, null, null, th, NIL);
            }
        } catch(exc) {
            // unwrap handler to prevent eval if exc is sym or cons
            var res = combine(e, null, null, unwrap(handler), list(exc));
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return self(e, k, f, o); }, th, e);
            return res;
        } else {
            return res;
        }
    };
    Finally.prototype.wat_combine = function self(e, k, f, o) {
        var prot = elt(o, 0);
        var cleanup = elt(o, 1);
        try {
            if (isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, prot);
            }
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, prot, e);
            }
        } finally {
            if (isCapture(res)) {
                return res;
            } else {
                return doCleanup(e, null, null, cleanup, res);
            }
        }
    };
    function doCleanup(e, k, f, cleanup, res) {
        if (isContinuation(k)) {
            var fres = continueFrame(k, f);
        } else {
            var fres = evaluate(e, null, null, cleanup);
        }
        if (isCapture(fres)) {
            captureFrame(fres, function(k, f) { return doCleanup(e, k, f, cleanup, res); }, cleanup, e);
            return fres;
        } else {
            return res;
        }
    }
    /* Delimited Control */
    function __PushPrompt() {}; function __TakeSubcont() {}; function __PushSubcont() {}
    __PushPrompt.prototype.wat_combine = function self(e, k, f, o) {
        var prompt = elt(o, 0);
        var x = elt(o, 1);
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = evaluate(e, null, null, x);
        }
        if (isCapture(res)) {
            if (res.prompt === prompt) {
                var continuation = res.k;
                var handler = res.handler;
                return combine(e, null, null, handler, cons(continuation, NIL));
            } else {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, x, e);
                return res;
            }
        } else {
            return res;
        }
    };
    __TakeSubcont.prototype.wat_combine = function(e, k, f, o) {
        var prompt = elt(o, 0);
        var handler = elt(o, 1);
        var cap = new Capture(prompt, handler);
        captureFrame(cap, function(k, thef) { return combine(e, null, null, thef, NIL); }, this, e);
        return cap;
    };
    __PushSubcont.prototype.wat_combine = function self(e, k, f, o) {
        var thek = elt(o, 0);
        var thef = elt(o, 1);
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = continueFrame(thek, thef);
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return self(e, k, f, o); }, thef, e);
            return res;
        } else {
            return res;
        }
    };
    /* Dynamic Variables */
    function DV(val) { this.val = val; }
    function DNew() {}; function DRef() {}; function __DLet() {}
    DNew.prototype.wat_combine = function(e, k, f, o) { return new DV(elt(o, 0)); };
    DRef.prototype.wat_combine = function(e, k, f, o) { return elt(o, 0).val; };
    __DLet.prototype.wat_combine = function self(e, k, f, o) {
        var dv = elt(o, 0);
        var val = elt(o, 1);
        var th = elt(o, 2);
        var oldVal = dv.val;
        dv.val = val;
        try {
            if (isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = combine(e, null, null, th, NIL);
            }
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, th, e);
                return res;
            } else {
                return res;
            }
        } finally {
            dv.val = oldVal;
        }
    };
    /* Objects */
    function Nil() {}; var NIL = new Nil();
    Nil.prototype.toString = function() { return "()"; };
    function Ign() {}; var IGN = new Ign();
    Ign.prototype.toString = function() { return "#ignore"; };
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) {
        if (cons instanceof Cons) return cons.car; else return fail("not a cons: " + to_string(cons)); }
    function cdr(cons) {
        if (cons instanceof Cons) return cons.cdr; else return fail("not a cons: " + to_string(cons)); }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function sym_name(sym) { return sym.name; }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function make_env(parent) { return new Env(parent); }
    function lookup(e, name) {
        if (name in e.bindings) return e.bindings[name];
        else return fail("unbound: " + name);
    }
    function bind(e, lhs, rhs) { return lhs.wat_match(e, rhs); }
    Sym.prototype.wat_match = function(e, rhs) {
        return e.bindings[this.name] = rhs; }
    Cons.prototype.wat_match = function(e, rhs) {
        var carCap = car(this).wat_match(e, car(rhs));
        if (isCapture(carCap)) return carCap;
        var cdrCap = cdr(this).wat_match(e, cdr(rhs));
        if (isCapture(cdrCap)) return cdrCap;
    };
    Nil.prototype.wat_match = function(e, rhs) {
        if (rhs !== NIL) return fail("NIL expected, but got: " + to_string(rhs)); };
    Ign.prototype.wat_match = function(e, rhs) {};
    /* Utilities */
    var ROOT_PROMPT = new Sym("--root-prompt");
    function push_root_prompt(x) {
        return parse_json_value(["push-prompt", ["quote", ROOT_PROMPT], x]); }
    function fail(err) {
        var handler = jswrap(function(k) {
            do {
                console.log(k.dbg ? to_string(k.dbg) : "[unknown stack frame]", k.e.bindings);
            } while((k = k.next) !== null);
            throw err;
        });
        var cap = new Capture(ROOT_PROMPT, handler);
        captureFrame(cap, function(k, f) { throw "never reached"; }, "[error handler stack frame]", {});
        return cap;
    }
    function list() {
        return array_to_list(Array.prototype.slice.call(arguments)); }
    function list_star() {
        var len = arguments.length; var c = len >= 1 ? arguments[len-1] : NIL;
        for (var i = len-1; i > 0; i--) c = cons(arguments[i - 1], c); return c; }
    function array_to_list(array, end) {
        var c = end ? end : NIL;
        for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c); return c; }
    function list_to_array(c) {
        var res = []; while(c !== NIL) { res.push(car(c)); c = cdr(c); } return res; }
    function reverse_list(list) {
        var res = NIL; while(list !== NIL) { res = cons(car(list), res); list = cdr(list); } return res; }
    function to_string(obj) {
        if ((obj !== null) && (obj !== undefined)) return obj.toString();
        else return Object.prototype.toString.call(obj); }
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
    function JSFun(jsfun) {
        if (Object.prototype.toString.call(jsfun) !== "[object Function]") return fail("no fun");
        this.jsfun = jsfun; }
    JSFun.prototype.wat_combine = function(e, k, f, o) {
        return this.jsfun.apply(null, list_to_array(o)); };
    JSFun.prototype.toString = function() { return "[JSFun " + this.jsfun.toString() + "]"; };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")); }
    function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")); }
    function js_invoker(method_name) {
        return jswrap(function() {
            if (arguments.length < 1) return fail("invoker: " + arguments);
            var rcv = arguments[0];
            var method = rcv[method_name];
            return method.apply(rcv, Array.prototype.slice.call(arguments, 1));
        }); }
    function js_getter(prop_name) {
        return jswrap(function() {
            if (arguments.length !== 1) return fail("getter: " + arguments);
            var rcv = arguments[0];
            return rcv[prop_name];
        }); }
    function js_setter(prop_name) {
        return jswrap(function() {
            if (arguments.length !== 2) return fail("setter: " + arguments);
            var rcv = arguments[0];
            return rcv[prop_name] = arguments[1];
        }); }
    function js_callback(cmb) {
        return function() {
            var args = array_to_list(Array.prototype.slice.call(arguments));
            return evaluate(environment, null, null, push_root_prompt(cons(cmb, args)));
        } }
    /* Primitives */
    var primitives =
        ["begin",

         // Core

         // Fexprs
         ["def", "--vau", new __Vau()],
         ["def", "eval", wrap(new Eval())],
         ["def", "make-environment", jswrap(function() { return make_env(); })],
         ["def", "wrap", jswrap(wrap)],
         ["def", "unwrap", jswrap(unwrap)],
         // Values
         ["def", "cons", jswrap(cons)],
         ["def", "cons?", jswrap(function(obj) { return obj instanceof Cons; })],
         ["def", "nil?", jswrap(function(obj) { return obj === NIL; })],
         ["def", "symbol?", jswrap(function(obj) { return obj instanceof Sym; })],
         ["def", "symbol-name", jswrap(sym_name)],
         // First-order Control
         ["def", "if", new If()],
         ["def", "--loop", new __Loop()],
         ["def", "throw", jswrap(function(err) { throw err; })],
         ["def", "--catch", wrap(new __Catch())],
         ["def", "finally", new Finally()],
         // Delimited Control
         ["def", "--push-prompt", new __PushPrompt()],
         ["def", "--take-subcont", wrap(new __TakeSubcont())],
         ["def", "--push-subcont", wrap(new __PushSubcont())],
         // Dynamically-scoped Variables
         ["def", "dnew", wrap(new DNew())],
         ["def", "--dlet", wrap(new __DLet())],
         ["def", "dref", wrap(new DRef())],
         // JS Interface
         ["def", "js-wrap", jswrap(jswrap)],
         ["def", "js-unop", jswrap(js_unop)],
         ["def", "js-binop", jswrap(js_binop)],
         ["def", "js-getter", jswrap(js_getter)],
         ["def", "js-setter", jswrap(js_setter)],
         ["def", "js-invoker", jswrap(js_invoker)],
         ["def", "js-callback", jswrap(js_callback)],
         ["def", "list-to-array", jswrap(list_to_array)],
         ["def", "array-to-list", jswrap(array_to_list)],
         // Optimization
         ["def", "list*", jswrap(list_star)],

         // Primitives

         ["def", "quote", ["--vau", ["x"], "#ignore", "x"]],
         ["def", "list", ["wrap", ["--vau", "arglist", "#ignore", "arglist"]]],
         ["def", "string", ["--vau", ["sym"], "#ignore", ["symbol-name", "sym"]]],
         ["def", "get-current-environment", ["--vau", [], "e", "e"]],

         ["def", "make-macro-expander",
          ["wrap",
           ["--vau", ["expander"], "#ignore",
            ["--vau", "operands", "env",
             ["eval", ["eval", ["cons", "expander", "operands"], ["make-environment"]], "env"]]]]],

         ["def", "vau",
          ["make-macro-expander",
           ["--vau", ["params", "env-param", "#rest", "body"], "#ignore",
            ["list", "--vau", "params", "env-param", ["cons", "begin", "body"]]]]],

         ["def", "macro",
          ["make-macro-expander",
           ["vau", ["params", "#rest", "body"], "#ignore",
            ["list", "make-macro-expander", ["list*", "vau", "params", "#ignore", "body"]]]]],

         ["def", "lambda",
          ["macro", ["params", "#rest", "body"],
           ["list", "wrap", ["list*", "vau", "params", "#ignore", "body"]]]],
         ["def", "loop",
          ["macro", "body",
           ["list", "--loop", ["list*", "begin", "body"]]]],
         ["def", "catch",
          ["macro", ["protected", "handler"],
           ["list", "--catch", ["list", "lambda", [], "protected"], "handler"]]],

         ["def", "push-prompt",
          ["vau", ["prompt", "#rest", "body"], "e",
           ["eval", ["list", "--push-prompt", ["eval", "prompt", "e"], ["list*", "begin", "body"]], "e"]]],
         ["def", "take-subcont",
          ["macro", ["prompt", "k", "#rest", "body"],
           ["list", "--take-subcont", "prompt", ["list*", "lambda", ["list", "k"], "body"]]]],
         ["def", "push-subcont",
          ["macro", ["k", "#rest", "body"],
           ["list", "--push-subcont", "k", ["list*", "lambda", [], "body"]]]],

         ["def", "dlet",
          ["macro", ["dv", "val", "#rest", "body"],
           ["list", "--dlet", "dv", "val", ["list*", "lambda", [], "body"]]]],

         // JS

         ["def", "array", ["lambda", "args", ["list-to-array", "args"]]],

         ["def", "define-js-unop",
          ["macro", ["op"],
           ["list", "def", "op", ["list", "js-unop", ["list", "string", "op"]]]]],

         ["define-js-unop", "!"],
         ["define-js-unop", "typeof"],
         ["define-js-unop", "~"],

         ["def", "define-js-binop",
          ["macro", ["op"],
           ["list", "def", "op", ["list", "js-binop", ["list", "string", "op"]]]]],

         ["define-js-binop", "!="],
         ["define-js-binop", "!=="],
         ["define-js-binop", "%"],
         ["define-js-binop", "&"],
         ["define-js-binop", "&&"],
         ["define-js-binop", "*"],
         ["define-js-binop", "+"],
         ["define-js-binop", "-"],
         ["define-js-binop", "/"],
         ["define-js-binop", "<"],
         ["define-js-binop", "<<"],
         ["define-js-binop", "<="],
         ["define-js-binop", "=="],
         ["define-js-binop", "==="],
         ["define-js-binop", ">"],
         ["define-js-binop", ">>"],
         ["define-js-binop", ">>>"],
         ["define-js-binop", "^"],
         ["define-js-binop", "in"],
         ["define-js-binop", "instanceof"],
         ["define-js-binop", "|"],
         ["define-js-binop", "||"],

         ["def", ".",
          ["macro", ["field", "obj"],
           ["list", ["list", "js-getter", ["list", "string", "field"]], "obj"]]],

         ["def", "#",
          ["macro", ["method", "obj", "#rest", "args"],
           ["list*", ["list", "js-invoker", ["list", "string", "method"]], "obj", "args"]]],

        ];
    /* Init */
    var environment = make_env();
    bind(environment, new Sym("def"), new Def());
    bind(environment, new Sym("begin"), new Begin());
    evaluate(environment, null, null, parse_json_value(primitives));
    /* API */
    function run(x) {
        var wrapped = push_root_prompt(parse_json_value(x));
        return evaluate(environment, null, null, wrapped);
    }
    return { "run": run };
}
})(typeof exports === "undefined" ? this["wat"] = {} : exports);
