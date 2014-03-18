(function(wat, global) {
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
        return JSON.stringify(cons);
    }
    /* Operative & Applicative Combiners */
    function combine(e, k, f, cmb, o) {
        if (cmb && cmb.wat_combine) return cmb.wat_combine(e, k, f, o);
        else if (cmb instanceof Function) return jswrap(cmb).wat_combine(e, k, f, o);
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
        return list(new Sym("push-prompt"), list(new Sym("quote"), ROOT_PROMPT), x); }
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
    /* JSON parser */
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
    /* S-expr parser */
    function parse_sexp(s) {
        var res = program_stx(ps(s));
        if (res.remaining.index === s.length) return cons(new Sym("begin"), array_to_list(res.ast));
        else throw("parse error at " + res.remaining.index + " in " + s); }
    var x_stx = function(input) { return x_stx(input); }; // forward decl.
    var id_special_char =
        choice("-", "&", "!", ":", "=", ">", "<", "%", "+", "?", "/", "*", "#", "$", "_", "'", ".", "@");
    var id_char = choice(range("a", "z"), range("A", "Z"), range("0", "9"), id_special_char);
    // Kludge: don't allow single dot as id, so as not to conflict with dotted pair stx.
    var id_stx = action(join_action(butnot(repeat1(id_char), "."), ""), function (ast) {
        if (ast[0] === ".") { return list(new Sym("js-getter"), ast.substring(1)) }
        else if (ast[0] === "#") { return list(new Sym("js-invoker"), ast.substring(1)) }
        else if (ast[0] === "@") { return list(new Sym("js-global"), ast.substring(1)) }
        else return new Sym(ast);
    });
    var escape_char = choice("\"", "\\");
    var escape_sequence = action(sequence("\\", escape_char), function (ast) { return ast[1]; });
    var string_char = choice(negate(escape_char), escape_sequence);
    var string_stx =
        action(sequence("\"", join_action(repeat0(string_char), ""), "\""),
               function (ast) { return ast[1]; });
    var digits = join_action(repeat1(range("0", "9")), "");
    var number_stx =
        action(sequence(optional(choice("+", "-")), digits, optional(join_action(sequence(".", digits), ""))),
               function (ast) {
                   var sign = ast[0] ? ast[0] : "";
                   var integral_digits = ast[1]; 
                   var fractional_digits = ast[2] || "";
                   return Number(sign + integral_digits + fractional_digits); });
    function make_constant_stx(string, constant) { return action(string, function(ast) { return constant; }); }
    var ign_stx = make_constant_stx("ignore", IGN);
    var nil_stx = make_constant_stx("()", NIL);
    var t_stx = make_constant_stx("true", true);
    var f_stx = make_constant_stx("false", false);
    var null_stx = make_constant_stx("null", null);
    var undef_stx = make_constant_stx("undefined", undefined);
    var dot_stx = action(wsequence(".", x_stx), function (ast) { return ast[1]; });
    var compound_stx = action(wsequence("(", repeat1(x_stx), optional(dot_stx), ")"),
                              function(ast) {
                                  var exprs = ast[1];
                                  var end = ast[2] ? ast[2] : NIL;
                                  return array_to_list(exprs, end); });
    var line_terminator = choice(ch("\r"), ch("\n"));
    var cmt_stx = action(sequence(";", repeat0(negate(line_terminator)), optional(line_terminator)), nothing_action);
    var whitespace_stx = action(choice(" ", "\n", "\r", "\t"), nothing_action);
    function nothing_action(ast) { return "void"; } // HACK!
    var x_stx = whitespace(choice(ign_stx, nil_stx, t_stx, f_stx, null_stx, undef_stx, number_stx,
                                  compound_stx, id_stx, string_stx, cmt_stx));
    var program_stx = whitespace(repeat0(choice(x_stx, whitespace_stx))); // HACK!
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
         ["def", "js-global", jswrap(function(name) { return global[name]; })],
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

         // Core Language

         ["def", "compose",
          ["lambda", ["f", "g"], ["lambda", ["arg"], ["f", ["g", "arg"]]]]],

         ["def", "car", ["lambda", [["x", "#rest", "#ignore"]], "x"]],
         ["def", "cdr", ["lambda", [["#ignore", "#rest", "x"]], "x"]],
         ["def", "caar", ["compose", "car", "car"]],
         ["def", "cadr", ["compose", "car", "cdr"]],
         ["def", "cdar", ["compose", "cdr", "car"]],
         ["def", "cddr", ["compose", "cdr", "cdr"]],

         ["def", "define-macro",
          ["macro", [["name", "#rest", "params"], "#rest", "body"],
           ["list", "def", "name", ["list*", "macro", "params", "body"]]]],

         ["define-macro", ["define", "lhs", "#rest", "rhs"],
          ["if", ["cons?", "lhs"],
           ["list", "def", ["car", "lhs"], ["list*", "lambda", ["cdr", "lhs"], "rhs"]],
           ["list", "def", "lhs", ["car", "rhs"]]]],

         ["define", ["map-list", "f", "lst"],
           ["if", ["nil?", "lst"],
            [],
            ["cons", ["f", ["car", "lst"]], ["map-list", "f", ["cdr", "lst"]]]]],

         ["define-macro", ["let", "bindings", "#rest", "body"],
          ["cons",
           ["list*", "lambda", ["map-list", "car", "bindings"], "body"],
           ["map-list", "cadr", "bindings"]]],

         ["define-macro", ["let*", "bindings", "#rest", "body"],
          ["if", ["nil?", "bindings"],
           ["list*", "let", [], "body"],
           ["list", "let", ["list", ["car", "bindings"]],
            ["list*", "let*", ["cdr", "bindings"], "body"]]]],

         ["define-macro", ["where", "expr", "#rest", "bindings"],
          ["list", "let", "bindings", "expr"]],

         ["define-macro", ["where*", "expr", "#rest", "bindings"],
          ["list", "let*", "bindings", "expr"]],

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

         ["def", "set!",
          ["vau", ["env", "lhs", "rhs"], "denv",
           ["eval",
            ["list", "def", "lhs",
             ["list", ["unwrap", "eval"], "rhs", "denv"]],
            ["eval", "env", "denv"]]]],

         ["define", ["apply", "appv", "arg"],
          ["eval", ["cons", ["unwrap", "appv"], "arg"], ["make-environment"]]]

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
    function eval(sexp) {
        var wrapped = push_root_prompt(parse_sexp(sexp));
        return evaluate(environment, null, null, wrapped);
    }
    return { "run": run, "eval": eval };
}
// Copyright (C) 2007 Chris Double.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// DEVELOPERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

function foldl(f, initial, seq) {
    for(var i=0; i< seq.length; ++i)
        initial = f(initial, seq[i]);
    return initial;
}

var memoize = true;

function ParseState(input, index) {
    this.input = input;
    this.index = index || 0;
    this.length = input.length - this.index;
    this.cache = { };
    return this;
}

ParseState.prototype.from = function(index) {
    var r = new ParseState(this.input, this.index + index);
    r.cache = this.cache;
    r.length = this.length - index;
    return r;
}

ParseState.prototype.substring = function(start, end) {
    return this.input.substring(start + this.index, (end || this.length) + this.index);
}

ParseState.prototype.trimLeft = function() {
    var s = this.substring(0);
    var m = s.match(/^\s+/);
    return m ? this.from(m[0].length) : this;
}

ParseState.prototype.at = function(index) {
    return this.input.charAt(this.index + index);
}

ParseState.prototype.toString = function() {
    return 'PS"' + this.substring(0) + '"';
}

ParseState.prototype.getCached = function(pid) {
    if(!memoize)
        return false;

    var p = this.cache[pid];
    if(p)
        return p[this.index];
    else
        return false;
}

ParseState.prototype.putCached = function(pid, cached) {
    if(!memoize)
        return false;

    var p = this.cache[pid];
    if(p)
        p[this.index] = cached;
    else {
        p = this.cache[pid] = { };
        p[this.index] = cached;
    }
}

function ps(str) {
    return new ParseState(str);
}

// 'r' is the remaining string to be parsed.
// 'matched' is the portion of the string that
// was successfully matched by the parser.
// 'ast' is the AST returned by the successfull parse.
function make_result(r, matched, ast) {
        return { remaining: r, matched: matched, ast: ast };
}

var parser_id = 0;

// 'token' is a parser combinator that given a string, returns a parser
// that parses that string value. The AST contains the string that was parsed.
function token(s) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var r = state.length >= s.length && state.substring(0,s.length) == s;
        if(r)
            cached = { remaining: state.from(s.length), matched: s, ast: s };
        else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Like 'token' but for a single character. Returns a parser that given a string
// containing a single character, parses that character value.
function ch(c) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = state.length >= 1 && state.at(0) == c;
        if(r)
            cached = { remaining: state.from(1), matched: c, ast: c };
        else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    };
}

// 'range' is a parser combinator that returns a single character parser
// (similar to 'ch'). It parses single characters that are in the inclusive
// range of the 'lower' and 'upper' bounds ("a" to "z" for example).
function range(lower, upper) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        if(state.length < 1)
            cached = false;
        else {
            var ch = state.at(0);
            if(ch >= lower && ch <= upper)
                cached = { remaining: state.from(1), matched: ch, ast: ch };
            else
                cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Helper function to convert string literals to token parsers
// and perform other implicit parser conversions.
function toParser(p) {
    return (typeof(p) == "string") ? token(p) : p;
}

// Parser combinator that returns a parser that
// skips whitespace before applying parser.
function whitespace(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        cached = p(state.trimLeft());
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Parser combinator that passes the AST generated from the parser 'p'
// to the function 'f'. The result of 'f' is used as the AST in the result.
function action(p, f) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var x = p(state);
        if(x) {
            x.ast = f(x.ast);
            cached = x;
        }
        else {
            cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Given a parser that produces an array as an ast, returns a
// parser that produces an ast with the array joined by a separator.
function join_action(p, sep) {
    return action(p, function(ast) { return ast.join(sep); });
}

// Given an ast of the form [ Expression, [ a, b, ...] ], convert to
// [ [ [ Expression [ a ] ] b ] ... ]
// This is used for handling left recursive entries in the grammar. e.g.
// MemberExpression:
//   PrimaryExpression
//   FunctionExpression
//   MemberExpression [ Expression ]
//   MemberExpression . Identifier
//   new MemberExpression Arguments
function left_factor(ast) {
    return foldl(function(v, action) {
                     return [ v, action ];
                 },
                 ast[0],
                 ast[1]);
}

// Return a parser that left factors the ast result of the original
// parser.
function left_factor_action(p) {
    return action(p, left_factor);
}

// 'negate' will negate a single character parser. So given 'ch("a")' it will successfully
// parse any character except for 'a'. Or 'negate(range("a", "z"))' will successfully parse
// anything except the lowercase characters a-z.
function negate(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        if(state.length >= 1) {
            var r = p(state);
            if(!r)
                cached =  make_result(state.from(1), state.at(0), state.at(0));
            else
                cached = false;
        }
        else {
            cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
}

// 'end_p' is a parser that is successful if the input string is empty (ie. end of parse).
function end_p(state) {
    if(state.length == 0)
        return make_result(state, undefined, undefined);
    else
        return false;
}

// 'nothing_p' is a parser that always fails.
function nothing_p(state) {
    return false;
}

// 'sequence' is a parser combinator that processes a number of parsers in sequence.
// It can take any number of arguments, each one being a parser. The parser that 'sequence'
// returns succeeds if all the parsers in the sequence succeeds. It fails if any of them fail.
function sequence() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i]));
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }

        var ast = [];
        var matched = "";
        var i;
        for(i=0; i< parsers.length; ++i) {
            var parser = parsers[i];
            var result = parser(state);
            if(result) {
                state = result.remaining;
                if(result.ast != undefined) {
                    ast.push(result.ast);
                    matched = matched + result.matched;
                }
            }
            else {
                break;
            }
        }
        if(i == parsers.length) {
            cached = make_result(state, matched, ast);
        }
        else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Like sequence, but ignores whitespace between individual parsers.
function wsequence() {
    var parsers = [];
    for(var i=0; i < arguments.length; ++i) {
        parsers.push(whitespace(toParser(arguments[i])));
    }
    return sequence.apply(null, parsers);
}

// 'choice' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that succeeds results in a
// successfull parse. It fails if all parsers fail.
function choice() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i]));
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }
        var i;
        for(i=0; i< parsers.length; ++i) {
            var parser=parsers[i];
            var result = parser(state);
            if(result) {
                break;
            }
        }
        if(i == parsers.length)
            cached = false;
        else
            cached = result;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// 'butnot' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not, or
// 'p1' matches and the matched text is longer that p2's.
// Useful for things like: butnot(IdentifierName, ReservedWord)
function butnot(p1,p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a but not b. if both match and b's matched text is shorter
    // than a's, a failed match is made
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var br = p2(state);
        if(!br) {
            cached = p1(state);
        } else {
            var ar = p1(state);

            if (ar) {
              if(ar.matched.length > br.matched.length)
                  cached = ar;
              else
                  cached = false;
            }
            else {
              cached = false;
            }
        }
        savedState.putCached(pid, cached);
        return cached;
    }
}

// 'difference' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not. If
// both match then if p2's matched text is shorter than p1's it is successfull.
function difference(p1,p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a but not b. if both match and b's matched text is shorter
    // than a's, a successfull match is made
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var br = p2(state);
        if(!br) {
            cached = p1(state);
        } else {
            var ar = p1(state);
            if(ar.matched.length >= br.matched.length)
                cached = br;
            else
                cached = ar;
        }
        savedState.putCached(pid, cached);
        return cached;
    }
}


// 'xor' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' or 'p2' match but fails if
// they both match.
function xor(p1, p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a or b but not both
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var ar = p1(state);
        var br = p2(state);
        if(ar && br)
            cached = false;
        else
            cached = ar || br;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that takes one parser. It returns a parser that
// looks for zero or more matches of the original parser.
function repeat0(p) {
    var p = toParser(p);
    var pid = parser_id++;

    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }

        var ast = [];
        var matched = "";
        var result;
        while(result = p(state)) {
            ast.push(result.ast);
            matched = matched + result.matched;
            if(result.remaining.index == state.index)
                break;
            state = result.remaining;
        }
        cached = make_result(state, matched, ast);
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that takes one parser. It returns a parser that
// looks for one or more matches of the original parser.
function repeat1(p) {
    var p = toParser(p);
    var pid = parser_id++;

    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var ast = [];
        var matched = "";
        var result= p(state);
        if(!result)
            cached = false;
        else {
            while(result) {
                ast.push(result.ast);
                matched = matched + result.matched;
                if(result.remaining.index == state.index)
                    break;
                state = result.remaining;
                result = p(state);
            }
            cached = make_result(state, matched, ast);
        }
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that takes one parser. It returns a parser that
// matches zero or one matches of the original parser.
function optional(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r || make_result(state, "", false);
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that ensures that the given parser succeeds but
// ignores its result. This can be useful for parsing literals that you
// don't want to appear in the ast. eg:
// sequence(expect("("), Number, expect(")")) => ast: Number
function expect(p) {
    return action(p, function(ast) { return undefined; });
}

function chain(p, s, f) {
    var p = toParser(p);

    return action(sequence(p, repeat0(action(sequence(s, p), f))),
                  function(ast) { return [ast[0]].concat(ast[1]); });
}

// A parser combinator to do left chaining and evaluation. Like 'chain', it expects a parser
// for an item and for a seperator. The seperator parser's AST result should be a function
// of the form: function(lhs,rhs) { return x; }
// Where 'x' is the result of applying some operation to the lhs and rhs AST's from the item
// parser.
function chainl(p, s) {
    var p = toParser(p);
    return action(sequence(p, repeat0(sequence(s, p))),
                  function(ast) {
                      return foldl(function(v, action) { return action[0](v, action[1]); }, ast[0], ast[1]);
                  });
}

// A parser combinator that returns a parser that matches lists of things. The parser to
// match the list item and the parser to match the seperator need to
// be provided. The AST is the array of matched items.
function list(p, s) {
    return chain(p, s, function(ast) { return ast[1]; });
}

// Like list, but ignores whitespace between individual parsers.
function wlist() {
    var parsers = [];
    for(var i=0; i < arguments.length; ++i) {
        parsers.push(whitespace(arguments[i]));
    }
    return list.apply(null, parsers);
}

// A parser that always returns a zero length match
function epsilon_p(state) {
    return make_result(state, "", undefined);
}

// Allows attaching of a function anywhere in the grammer. If the function returns
// true then parse succeeds otherwise it fails. Can be used for testing if a symbol
// is in the symbol table, etc.
function semantic(f) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        cached = f() ? make_result(state, "", undefined) : false;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// The and predicate asserts that a certain conditional
// syntax is satisfied before evaluating another production. Eg:
// sequence(and("0"), oct_p)
// (if a leading zero, then parse octal)
// It succeeds if 'p' succeeds and fails if 'p' fails. It never
// consume any input however, and doesn't put anything in the resulting
// AST.
function and(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r ? make_result(state, "", undefined) : false;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// The opposite of 'and'. It fails if 'p' succeeds and succeeds if
// 'p' fails. It never consumes any input. This combined with 'and' can
// be used for 'lookahead' and disambiguation of cases.
//
// Compare:
// sequence("a",choice("+","++"),"b")
//   parses a+b
//   but not a++b because the + matches the first part and peg's don't
//   backtrack to other choice options if they succeed but later things fail.
//
// sequence("a",choice(sequence("+", not("+")),"++"),"b")
//    parses a+b
//    parses a++b
//
function not(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        cached = p(state) ? false : make_result(state, "", undefined);
        savedState.putCached(pid, cached);
        return cached;
    }
}

})(typeof exports === "undefined" ? this["wat"] = {} : exports, this);
