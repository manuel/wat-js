(function(wat){
wat.VM = function() {
    /* Continuations */
    function Continuation(fun, next) {
        this.fun = fun; this.next = next; }
    function isContinuation(x) { return x instanceof Continuation; }
    function Capture(prompt, handler) {
        this.prompt = prompt; this.handler = handler; this.k = null; }
    function isCapture(x) { return x instanceof Capture; }
    function captureFrame(capture, fun) {
        capture.k = new Continuation(fun, capture.k); }
    function continueFrame(k, f) {
        return k.fun(k.next, f); }
    /* Evaluation Core */
    function evaluate(e, k, f, x) {
        if (x && x.wat_eval) return x.wat_eval(e, k, f); else return x; }
    function Sym(name) { this.name = name; }
    Sym.prototype.wat_eval = function(e, k, f) { return lookup(e, this.name); };
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    Cons.prototype.wat_eval = function(e, k, f) {
        if (isContinuation(k)) {
            var op = continueFrame(k, f);
        } else {
            var op = evaluate(e, null, null, car(this));
        }
        if (isCapture(op)) {
            var that = this;
            captureFrame(op, function(k, f) { return that.wat_eval(e, k, f); });
            return op;
        }
        return combine(e, null, null, op, cdr(this));
    };
    /* Operative & Applicative Combiners */
    function combine(e, k, f, cmb, o) {
        if (cmb && cmb.wat_combine)
            return cmb.wat_combine(e, k, f, o);
        else if (Object.prototype.toString.call(cmb) === "[object Function]")
            return combine(e, k, f, jswrap(cmb), o);
        else
            fail("not a function: " + JSON.stringify(cmb));
    }
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }
    function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; }
    Opv.prototype.wat_combine = function(e, k, f, o) {
        var xe = new Env(this.e); bind(xe, this.p, o); bind(xe, this.ep, e);
        return evaluate(xe, k, f, this.x);
    };
    Apv.prototype.wat_combine = function(e, k, f, o) {
        if (isContinuation(k)) {
            var args = continueFrame(k, f);
        } else {
            var args = evalArgs(e, null, null, o, NIL);
        }
        if (isCapture(args)) {
            var that = this;
            captureFrame(args, function(k, f) { return that.wat_combine(e, k, f, o); });
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
            captureFrame(arg, function(k, f) { return evalArgs(e, k, f, todo, done); });
            return arg;
        }
        return evalArgs(e, null, null, cdr(todo), cons(arg, done));
    }
    /* Built-in Combiners */
    function Vau() {}; function Def() {}; function Eval() {}
    Vau.prototype.wat_combine = function(e, k, f, o) {
        return new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e); };
    Def.prototype.wat_combine = function self(e, k, f, o) {
        if (isContinuation(k)) {
            var val = continueFrame(k, f);
        } else {
            var val = evaluate(e, null, null, elt(o, 1));
        }
        if (isCapture(val)) {
            captureFrame(val, function(k, f) { return self(e, k, f, o); });
            return val;
        }
        return bind(e, elt(o, 0), val);
    };
    Eval.prototype.wat_combine = function(e, k, f, o) {
        return evaluate(elt(o, 1), k, f, elt(o, 0)); };
    /* First-order Control */
    function Begin() {}; function If() {}; function Loop() {}
    function Catch() {}; function Finally() {}
    Begin.prototype.wat_combine = function(e, k, f, o) {
        if (o === NIL) return null; else return begin(e, k, f, o); };
    function begin(e, k, f, xs) {
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = evaluate(e, null, null, car(xs));
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return begin(e, k, f, xs); });
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
            captureFrame(test, function(k, f) { return self(e, k, f, o); });
            return test;
        }
        return evaluate(e, null, null, test ? elt(o, 1) : elt(o, 2));
    };
    Loop.prototype.wat_combine = function self(e, k, f, o) {
        var first = true; // only continue once
        while (true) {
            if (first && isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, elt(o, 0));
            }
            first = false;
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); });
                return res;
            }
        }
    };
    Catch.prototype.wat_combine = function self(e, k, f, o) {
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
            captureFrame(res, function(k, f) { return self(e, k, f, o); });
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
                captureFrame(res, function(k, f) { return self(e, k, f, o); });
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
            captureFrame(fres, function(k, f) { return doCleanup(e, k, f, cleanup, res); });
            return fres;
        } else {
            return res;
        }
    }
    /* Delimited Control */
    function PushPrompt() {}; function TakeSubcont() {}; function PushSubcont() {}
    PushPrompt.prototype.wat_combine = function self(e, k, f, o) {
        var prompt = elt(o, 0);
        var th = elt(o, 1);
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = combine(e, null, null, th, NIL);
        }
        if (isCapture(res)) {
            if (res.prompt === prompt) {
                var continuation = res.k;
                var handler = res.handler;
                return combine(e, null, null, handler, cons(continuation, NIL));
            } else {
                captureFrame(res, function(k, f) { return self(e, k, f, o); });
                return res;
            }
        } else {
            return res;
        }
    };
    TakeSubcont.prototype.wat_combine = function(e, k, f, o) {
        var prompt = elt(o, 0);
        var handler = elt(o, 1);
        var cap = new Capture(prompt, handler);
        captureFrame(cap, function(k, thef) { return combine(e, null, null, thef, NIL); });
        return cap;
    };
    PushSubcont.prototype.wat_combine = function self(e, k, f, o) {
        var thek = elt(o, 0);
        var thef = elt(o, 1);
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = continueFrame(thek, thef);
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return self(e, k, f, o); });
            return res;
        } else {
            return res;
        }
    };
    /* Dynamic Variables */
    function DV(val) { this.val = val; }
    function DNew() {}; function DLet() {}; function DRef() {}
    DNew.prototype.wat_combine = function(e, k, f, o) {
        return new DV(elt(o, 0));
    };
    DLet.prototype.wat_combine = function self(e, k, f, o) {
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
                captureFrame(res, function(k, f) { return self(e, k, f, o); });
                return res;
            } else {
                return res;
            }
        } finally {
            dv.val = oldVal;
        }
    };
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
    function lookup(e, name) {
        var val = e.bindings[name];
        return (typeof(val) !== "undefined") ? val : fail("unbound: " + name); }
    function bind(e, lhs, rhs) { lhs.match(e, rhs); return rhs; }
    Sym.prototype.match = function(e, rhs) {
        if (typeof(e) === "undefined") fail("undefined argument: " + this.name);
        return e.bindings[this.name] = rhs; }
    Cons.prototype.match = function(e, rhs) {
        car(this).match(e, car(rhs)); cdr(this).match(e, cdr(rhs)); };
    Nil.prototype.match = function(e, rhs) {
        if (rhs !== NIL) fail("NIL expected, but got: " + JSON.stringify(rhs)); };
    Ign.prototype.match = function(e, rhs) {};
    /* Utilities */
    function fail(err) { throw err; }
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
        if (Object.prototype.toString.call(jsfun) !== "[object Function]") fail("no fun");
        this.jsfun = jsfun; }
    JSFun.prototype.wat_combine = function(e, k, f, o) {
        return this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")); }
    function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")); }
    function js_invoke(obj, method_name) {
        return obj[method_name].apply(obj, Array.prototype.slice.call(arguments, 2)); }
    function JSCallback() {};
    JSCallback.prototype.wat_combine = function(e, k, f, o) {
        var cmb = elt(o, 0);
        return function() {
            var args = array_to_list(Array.prototype.slice.call(arguments));
            combine(e, null, null, cmb, args);
        }; };
    function sym_name(sym) { return sym.name; }
    /* Primitives */
    var primitives =
        ["wat-begin",

         // Core

         // Fexprs
         ["wat-def", "wat-vau", new Vau()],
         ["wat-def", "wat-eval", wrap(new Eval())],
         ["wat-def", "wat-make-environment", jswrap(function() { return new Env(); })],
         ["wat-def", "wat-wrap", jswrap(wrap)],
         // Forms
         ["wat-def", "wat-cons", jswrap(cons)],
         ["wat-def", "wat-cons?", jswrap(function(obj) { return obj instanceof Cons; })],
         ["wat-def", "wat-nil?", jswrap(function(obj) { return obj === NIL; })],
         ["wat-def", "wat-symbol-name", jswrap(sym_name)],
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
         ["wat-def", "wat-js-unop", new JSFun(function(sym) { return js_unop(sym_name(sym)); })],
         ["wat-def", "wat-js-binop", new JSFun(function(sym) { return js_binop(sym_name(sym)); })],
         ["wat-def", "wat-js-element", jswrap(function(obj, i) { return obj[i]; })],
         ["wat-def", "wat-js-set-element", jswrap(function(obj, i, v) { return obj[i] = v; })],
         ["wat-def", "wat-js-invoke", jswrap(js_invoke)],
         ["wat-def", "wat-js-callback", wrap(new JSCallback())],
         ["wat-def", "wat-list-to-array", jswrap(list_to_array)],
         // Optimization
         ["wat-def", "wat-list*", jswrap(list_star)],
         // Macros
         ["wat-def", "wat-macro",
          ["wat-wrap",
           ["wat-vau", ["expander"], "#ignore",
            ["wat-vau", "operands", "env",
             ["eval", ["eval", ["cons", "expander", "operands"], ["make-environment"]], "env"]]]]],

         // Primitives

         ["wat-def", "def", "wat-def"],
         ["def", "[]", "wat-js-element"],
         ["def", "[]=", "wat-js-set-element"],
         ["def", "begin", "wat-begin"],
         ["def", "cons", "wat-cons"],
         ["def", "cons?", "wat-cons?"],
         ["def", "eval", "wat-eval"],
         ["def", "finally", "wat-finally"],
         ["def", "if", "wat-if"],
         ["def", "js-callback", "wat-js-callback"],
         ["def", "js-wrap", "wat-js-wrap"],
         ["def", "list*", "wat-list*"],
         ["def", "make-environment", "wat-make-environment"],
         ["def", "nil?", "wat-nil?"],
         ["def", "throw", "wat-throw"],

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
          ["list", "wat-loop", ["list*", "begin", "body"]]],
         ["define-macro", ["catch", "protected", "handler"],
          ["list", "wat-catch", ["list", "lambda", [], "protected"], "handler"]],

         ["define-macro", ["push-prompt", "prompt", "#rest", "body"],
          ["list", "wat-push-prompt", "prompt", ["list*", "lambda", [], "body"]]],
         ["define-macro", ["take-subcont", "prompt", "k", "#rest", "body"],
          ["list", "wat-take-subcont", "prompt", ["list*", "lambda", ["list", "k"], "body"]]],
         ["define-macro", ["push-subcont", "k", "#rest", "body"],
          ["list", "wat-push-subcont", "k", ["list*", "lambda", [], "body"]]],

         ["def", "compose",
          ["lambda", ["f", "g"], ["lambda", ["arg"], ["f", ["g", "arg"]]]]],

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

         ["define-macro", ["string", "sym"],
          ["wat-symbol-name", "sym"]],
         ["define", ["array", "#rest", "args"],
          ["wat-list-to-array", "args"]],
         
         ["define", ["map-list", "f", "lst"],
           ["if", ["nil?", "lst"],
            [],
            ["cons", ["f", ["car", "lst"]], ["map-list", "f", ["cdr", "lst"]]]]],

         ["define-macro", ["let", "bindings", "#rest", "body"],
          ["cons",
           ["list*", "lambda", ["map-list", "car", "bindings"], "body"],
           ["map-list", "cadr", "bindings"]]],

         ["define", ["call-with-escape", "fun"],
          ["let", [["fresh", ["list", null]]],
           ["catch", ["fun", ["lambda", ["val"], ["throw", ["list", "fresh", "val"]]]],
            ["lambda", ["exc"],
             ["if", ["&&", ["cons?", "exc"], ["===", "fresh", ["car", "exc"]]],
              ["cadr", "exc"],
              ["throw", "exc"]]]]]],

         ["define-macro", ["let-escape", "name", "#rest", "body"],
          ["list", "call-with-escape", ["list*", "lambda", ["list", "name"], "body"]]],

         ["define", ["call-while", "test-fun", "body-fun"],
          ["let-escape", "return",
           ["loop",
            ["if", ["test-fun"],
             ["body-fun"],
             ["return", null]]]]],

         ["define-macro", ["while", "test", "#rest", "body"],
          ["list", "call-while",
           ["list", "lambda", [], "test"],
           ["list*", "lambda", [], "body"]]],

         ["define-macro", ["define-js-unop", "op"],
          ["list", "define", "op", ["list", "wat-js-unop", "op"]]],
         ["define-js-unop", "!"],
         ["define-js-unop", "typeof"],
         ["define-js-unop", "~"],

         ["define-macro", ["define-js-binop", "op"],
          ["list", "define", "op", ["list", "wat-js-binop", "op"]]],
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

         ["define-macro", ["define-js-function", "name", "js-fun"],
          ["list", "define", "name", ["list", "js-wrap", "js-fun"]]],

         ["define-macro", [".", "field", "obj"],
           ["list", "wat-js-element", "obj", ["wat-symbol-name", "field"]]],
         ["define-macro", ["#", "method", "obj", "#rest", "args"],
          ["list*", "wat-js-invoke", "obj", ["wat-symbol-name", "method"], "args"]],

        ];
    /* Init */
    var environment = new Env();
    bind(environment, new Sym("wat-def"), new Def());
    bind(environment, new Sym("wat-begin"), new Begin());
    run(primitives);
    /* API */
    function run(x) { return evaluate(environment, null, null, parse_json_value(x)); }
    return { "run": run };
}
})(typeof exports === "undefined" ? this["wat"] = {} : exports);
