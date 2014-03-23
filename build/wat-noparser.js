!function(e){if("object"==typeof exports)module.exports=e();else if("function"==typeof define&&define.amd)define(e);else{var f;"undefined"!=typeof window?f=window:"undefined"!=typeof global?f=global:"undefined"!=typeof self&&(f=self),f.wat=e()}}(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
module.exports.main = ["begin",["def","quote",["--vau",["x"],"#ignore","x"]],["def","list",["wrap",["--vau","arglist","#ignore","arglist"]]],["def","string",["--vau",["sym"],"#ignore",["symbol-name","sym"]]],["def","get-current-environment",["--vau",[],"e","e"]],["def","make-macro-expander",["wrap",["--vau",["expander"],"#ignore",["--vau","operands","env",["eval",["eval",["cons","expander","operands"],["make-environment"]],"env"]]]]],["def","vau",["make-macro-expander",["--vau",["params","env-param","&rest","body"],"#ignore",["list","--vau","params","env-param",["cons","begin","body"]]]]],["def","macro",["make-macro-expander",["vau",["params","&rest","body"],"#ignore",["list","make-macro-expander",["list*","vau","params","#ignore","body"]]]]],["def","lambda",["macro",["params","&rest","body"],["list","wrap",["list*","vau","params","#ignore","body"]]]],["def","loop",["macro","body",["list","--loop",["list*","begin","body"]]]],["def","catch",["macro",["protected","handler"],["list","--catch",["list","lambda",[],"protected"],"handler"]]],["def","push-prompt",["vau",["prompt","&rest","body"],"e",["eval",["list","--push-prompt",["eval","prompt","e"],["list*","begin","body"]],"e"]]],["def","take-subcont",["macro",["prompt","k","&rest","body"],["list","--take-subcont","prompt",["list*","lambda",["list","k"],"body"]]]],["def","push-subcont",["macro",["k","&rest","body"],["list","--push-subcont","k",["list*","lambda",[],"body"]]]],["def","dlet",["vau",["dv","val","&rest","body"],"e",["eval",["cons","--dlet",["list",["eval","dv","e"],["eval","val","e"],["list*","begin","body"]]],"e"]]],["def","array",["lambda","args",["list-to-array","args"]]],["def","define-js-unop",["macro",["op"],["list","def","op",["list","js-unop",["list","string","op"]]]]],["define-js-unop","!"],["define-js-unop","typeof"],["define-js-unop","~"],["def","define-js-binop",["macro",["op"],["list","def","op",["list","js-binop",["list","string","op"]]]]],["define-js-binop","!="],["define-js-binop","!=="],["define-js-binop","%"],["define-js-binop","&"],["define-js-binop","*"],["define-js-binop","+"],["define-js-binop","-"],["define-js-binop","/"],["define-js-binop","<"],["define-js-binop","<<"],["define-js-binop","<="],["define-js-binop","=="],["define-js-binop","==="],["define-js-binop",">"],["define-js-binop",">>"],["define-js-binop",">>>"],["define-js-binop","^"],["define-js-binop","in"],["define-js-binop","instanceof"],["define-js-binop","|"],["def","compose",["lambda",["f","g"],["lambda",["arg"],["f",["g","arg"]]]]],["def","car",["lambda",[["x","&rest","#ignore"]],"x"]],["def","cdr",["lambda",[["#ignore","&rest","x"]],"x"]],["def","caar",["compose","car","car"]],["def","cadr",["compose","car","cdr"]],["def","cdar",["compose","cdr","car"]],["def","cddr",["compose","cdr","cdr"]],["def","define-macro",["macro",[["name","&rest","params"],"&rest","body"],["list","def","name",["list*","macro","params","body"]]]],["define-macro",["define","lhs","&rest","rhs"],["if",["cons?","lhs"],["list","def",["car","lhs"],["list*","lambda",["cdr","lhs"],"rhs"]],["list","def","lhs",["car","rhs"]]]],["define",["map-list","f","lst"],["if",["nil?","lst"],[],["cons",["f",["car","lst"]],["map-list","f",["cdr","lst"]]]]],["define-macro",["let","bindings","&rest","body"],["cons",["list*","lambda",["map-list","car","bindings"],"body"],["map-list","cadr","bindings"]]],["define-macro",["let*","bindings","&rest","body"],["if",["nil?","bindings"],["list*","let",[],"body"],["list","let",["list",["car","bindings"]],["list*","let*",["cdr","bindings"],"body"]]]],["define-macro",["defun",["name","&rest","params"],"&rest","body"],["list","def","name",["list*","typed-lambda","params","body"]]],["def","typed-lambda",["macro",["params","&rest","body"],["let",[[["vau-list","typed-body"],["xform-typed-lambda","params","body"]]],["list","wrap",["list*","vau","vau-list","#ignore","typed-body"]]]]],["define-macro",["the","type","obj"],["list","--type-check",["symbol-name","type"],"type","obj"]],["def","Arguments","$Arguments"],["def","Array","$Array"],["def","Date","$Date"],["def","Function","$Function"],["def","Number","$Number"],["def","Object","$Object"],["def","RegExp","$RegExp"],["def","String","$String"],["define",["call-with-escape","fun"],["let",[["fresh",["list",null]]],["catch",["fun",["lambda","opt-arg",["throw",["list","fresh","opt-arg"]]]],["lambda",["exc"],["if",["&&",["cons?","exc"],["===","fresh",["car","exc"]]],["let",[["opt-arg",["cadr","exc"]]],["if",["cons?","opt-arg"],["car","opt-arg"],[]]],null,["throw","exc"]]]]]],["define-macro",["label","name","&rest","body"],["list","call-with-escape",["list*","lambda",["list","name"],"body"]]],["define",["call-while","test-fun","body-fun"],["label","return",["loop",["if",["test-fun"],["body-fun"],["return",null]]]]],["define-macro",["while","test","&rest","body"],["list","call-while",["list","lambda",[],"test"],["list*","lambda",[],"body"]]],["define-macro",["when","test","&rest","body"],["list","if","test",["list*","begin","body"],null]],["define-macro",["unless","test","&rest","body"],["list*","when",["list","!","test"],"body"]],["define-macro",["&&","a","b"],["list","if","a","b",false]],["define-macro",["||","a","b"],["list","if","a",true,"b"]],["define-macro",["=","name","value"],["list","--set!","name","value"]],["define",["cat","&rest","objects"],["#join",["list-to-array","objects"],["string",""]]],["define",["log","&rest","objects"],["apply","#log",["list*","$console","objects"]]],["define",["--print-stacktrace-and-throw","err"],["define",["print-frame","k"],["#log","$console",["#toString",[".dbg","k"]],[".e","k"]],["if",[".next","k"],["print-frame",[".next","k"]],null]],["take-subcont",["--get-root-prompt"],"k",["print-frame","k"],["push-prompt",["--get-root-prompt"],["push-subcont","k",["throw","err"]]]]],["define","object",["vau","pairs","e",["let",[["obj",["--make-object"]]],["map-list",["lambda",["pair"],["let",[["name",["eval",["car","pair"],"e"]],["value",["eval",["cadr","pair"],"e"]]],[["js-setter","name"],"obj","value"]]],"pairs"],"obj"]]],["define-macro",["define-prototype","name","prop-names"],["list","define","name",["list*","--make-prototype",["symbol-name","name"],["map-list","symbol-name","prop-names"]]]],["define",["--put-method","ctor","name","js-fun"],[["js-setter","name"],[".prototype","ctor"],"js-fun"]],["define-macro",["define-method",["name",["self","ctor"],"&rest","args"],"&rest","body"],["list","--put-method","ctor",["symbol-name","name"],["list","js-function",["list*","typed-lambda",["list*","self","args"],"body"]]]],["define-macro",["define-generic",["name","&rest","#ignore"]],["list","define","name",["lambda","args",["apply",["js-invoker",["symbol-name","name"]],"args"]]]],["define",["@","object","key"],[["js-getter","key"],"object"]],["define",["js-callback","fun"],["js-function",["lambda","args",["push-prompt",["--get-root-prompt"],["apply","fun","args"]]]]],["define",["map-array","f","arr"],["let*",[["i",0],["len",[".length","arr"]],["res",["array"]]],["while",["<","i","len"],["#push","res",["f",["@","arr","i"]]],["=","i",["+","i",1]]],"res"]],["define","provide",["vau",["symbolz","&rest","body"],"env",["eval",["list","def","symbolz",["list","let",[],["list*","begin","body"],["list*","list","symbolz"]]],"env"]]],["define","module",["vau",["exports","&rest","body"],"e",["let",[["env",["make-environment","e"]]],["eval",["list*","provide","exports","body"],"env"],"env"]]],["define","define-module",["vau",["name","exports","&rest","body"],"e",["eval",["list","define","name",["list*","module","exports","body"]],"e"]]],["define","import",["vau",["module","imports"],"e",["let*",[["m",["eval","module","e"]],["values",["map-list",["lambda",["import"],["eval","import","m"]],"imports"]]],["eval",["list","def","imports",["list*","list","values"]],"e"]]]],null,null]

},{}],2:[function(require,module,exports){
(function (global){
var stdlib = require("./build/stdlib.js");
var parser = require("./wat-parser.js");
// Wat VM by Manuel Simoni (msimoni@gmail.com)
module.exports.VM = function() {
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
    function sym(name) { return new Sym(name); }
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
    function cons_to_string(c) {
        if (cdr(c) === NIL) return to_string(car(c));
        else if (cdr(c) instanceof Cons) { return to_string(car(c)) + " " + cons_to_string(cdr(c)); }
        else return to_string(car(c)) + " . " + to_string(cdr(c));
    }
    /* Operative & Applicative Combiners */
    function combine(e, k, f, cmb, o) {
        if (cmb && cmb.wat_combine) return cmb.wat_combine(e, k, f, o);
        else if (cmb instanceof Function) return jswrap(cmb).wat_combine(e, k, f, o);
        else return error("not a combiner: " + to_string(cmb)); }
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }
    Apv.prototype.toString = function() { return "[Apv " + to_string(this.cmb) + "]"; };
    function wrap(cmb) { return new Apv(cmb); };
    function unwrap(apv) { return apv instanceof Apv ? apv.cmb : error("cannot unwrap: " + apv); }
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
    function __Vau() {}; function Def() {}; function Eval() {}; function __Set() {};
    __Vau.prototype.toString = function() { return "vau"; };
    Def.prototype.toString = function() { return "def"; };
    Eval.prototype.toString = function() { return "eval"; };
    __Set.prototype.toString = function() { return "--set!"; };
    __Vau.prototype.wat_combine = function(e, k, f, o) {
        return new Opv(xform_vau_list(elt(o, 0)), elt(o, 1), elt(o, 2), e); };
    Def.prototype.wat_combine = function self(e, k, f, o) { return def_set(bind, e, k, f, o); };
    __Set.prototype.wat_combine = function self(e, k, f, o) { return def_set(set, e, k, f, o); };
    function def_set(binder, e, k, f, o) {
        var lhs = elt(o, 0); if (isCapture(lhs)) return lhs;
        var rhs = elt(o, 1); if (isCapture(rhs)) return rhs;
        if (isContinuation(k)) {
            var val = continueFrame(k, f);
        } else {
            var val = evaluate(e, null, null, rhs);
        }
        if (isCapture(val)) {
            captureFrame(val, function(k, f) { return def_set(binder, e, k, f, o); }, rhs, e);
            return val;
        }
        return binder(e, lhs, val);
    }
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
    __PushPrompt.prototype.toString = function() { return "--push-prompt"; }
    __TakeSubcont.prototype.toString = function() { return "--take-subcont"; }
    __PushSubcont.prototype.toString = function() { return "--push-subcont"; }
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
        var x = elt(o, 2);
        var oldVal = dv.val;
        dv.val = val;
        try {
            if (isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, x);
            }
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, x, e);
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
    Ign.prototype.toString = function() { return "ignore"; };
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) {
        if (cons instanceof Cons) return cons.car; else return error("not a cons: " + to_string(cons)); }
    function cdr(cons) {
        if (cons instanceof Cons) return cons.cdr; else return error("not a cons: " + to_string(cons)); }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function sym_name(sym) { return sym.name; }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); this.parent = parent; }
    function make_env(parent) { return new Env(parent); }
    function lookup(e, name) {
        if (name in e.bindings) return e.bindings[name];
        else return error("unbound: " + name);
    }
    function bind(e, lhs, rhs) { 
        if (lhs.wat_match) return lhs.wat_match(e, rhs); else return error("cannot match against: " + lhs); }
    function set(e, sym, rhs) {
        if (!e) return error("tried to set nonexistent binding: " + sym);
        else if (Object.prototype.hasOwnProperty.call(e.bindings, sym.name)) return bind(e, sym, rhs);
        else return set(e.parent, sym, rhs); }
    Sym.prototype.wat_match = function(e, rhs) {
        return e.bindings[this.name] = rhs; }
    Cons.prototype.wat_match = function(e, rhs) {
        var carCap = car(this).wat_match(e, car(rhs));
        if (isCapture(carCap)) return carCap;
        var cdrCap = cdr(this).wat_match(e, cdr(rhs));
        if (isCapture(cdrCap)) return cdrCap;
    };
    Nil.prototype.wat_match = function(e, rhs) {
        if (rhs !== NIL) return error("NIL expected, but got: " + to_string(rhs)); };
    Ign.prototype.wat_match = function(e, rhs) {};
    /* Utilities */
    var ROOT_PROMPT = sym("--root-prompt");
    function push_root_prompt(x) {
        return list(sym("push-prompt"), list(sym("quote"), ROOT_PROMPT), x); }
    function error(err) {
        var print_stacktrace = environment.bindings["--print-stacktrace-and-throw"];
        if (print_stacktrace !== undefined) {
            return combine(environment, null, null, print_stacktrace, list(err));
        } else {
            throw err;
        } }
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
    // Strip &rest from vau list, return (possibly) improper list
    function xform_vau_list(obj) {
        if (obj instanceof Cons) {
            var kar = car(obj);
            if ((kar instanceof Sym) && (kar.name === "&rest")) { return xform_vau_list(car(cdr(obj))); }
            else { return cons(xform_vau_list(kar), xform_vau_list(cdr(obj))); }
        } else { return obj; } }
    // Return (untyped-vau-list type-checked-body)
    function xform_typed_lambda(obj, body) {
        if ((obj === NIL) || (obj instanceof Sym)) { return list(obj, body); }
        var vau_list = [];
        var param_checks = [];
        var result_type = null;
        while(obj !== NIL) {
            var param = car(obj);
            if (param instanceof Cons) {
                var name = car(param); var type = car(cdr(param));
                vau_list.push(name);
                param_checks.push(list(sym("the"), type, name));
            } else if (param instanceof Sym) {
                if (param.name === "->") {
                    result_type = car(cdr(obj));
                    break;
                } else { vau_list.push(param); }
            } else if (param === IGN) {
                vau_list.push(param);
            } else { return error("param must be cons or sym: " + param); }
            obj = cdr(obj); }
        param_checks.map(function(ptc) { body = cons(ptc, body); });
        if (result_type !== null) { body = list(sym("the"), result_type, cons(sym("begin"), body)); }
        return list(array_to_list(vau_list), body); }
    var js_types = ["Arguments", "Array", "Date", "Function", "Number", "Object", "RegExp", "String"];
    function is_type(type_name, type_obj, obj) {
        if (js_types.indexOf(type_name) === -1) { return obj instanceof type_obj; }
        else { return toString.call(obj) === "[object " + type_name + "]"; } }
    function type_check(type_name, type_obj, obj) {
        if (!is_type(type_name, type_obj, obj)) {
            return error("type error: " + obj + " is not a " + type_name);
        } else { return obj; } }
    /* JSON parser */
    function parse_json_value(obj) {
        switch(Object.prototype.toString.call(obj)) {
        case "[object String]": return obj === "#ignore" ? IGN : handle_identifier(obj);
        case "[object Array]": return parse_json_array(obj);
        default: return obj; } }
    function handle_identifier(str) {
        if (str[0] === ".") { return list(sym("js-getter"), str.substring(1)); }
        else if (str[0] === "#") { return list(sym("js-invoker"), str.substring(1)); }
        else if (str[0] === "$") { return list(sym("js-global"), str.substring(1)); }
        else return sym(str); }
    function parse_json_array(arr) { return array_to_list(arr.map(parse_json_value)); }
    /* JSNI */
    function JSFun(jsfun) {
        if (Object.prototype.toString.call(jsfun) !== "[object Function]") return error("no fun");
        this.jsfun = jsfun; }
    JSFun.prototype.wat_combine = function(e, k, f, o) {
        return this.jsfun.apply(null, list_to_array(o)); };
    JSFun.prototype.toString = function() { return "[JSFun " + this.jsfun.toString() + "]"; };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")); }
    function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")); }
    function js_invoker(method_name) {
        return jswrap(function() {
            if (arguments.length < 1) return error("invoker called with wrong args: " + arguments);
            if (!method_name) return error("method name is null/undefined");
            var rcv = arguments[0];
            if (!rcv) return error("receiver is null/undefined");
            var method = rcv[method_name];
            return method.apply(rcv, Array.prototype.slice.call(arguments, 1));
        }); }
    function js_getter(prop_name) {
        return jswrap(function() {
            if (arguments.length !== 1) return error("getter called with wrong args: " + arguments);
            var rcv = arguments[0];
            if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name];
            else return error("can't get " + prop_name + " of " + rcv);
        }); }
    function js_setter(prop_name) {
        return jswrap(function() {
            if (arguments.length !== 2) return error("setter called with wrong args: " + arguments);
            var rcv = arguments[0];
            if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name] = arguments[1];
            else return error("can't set " + prop_name + " of " + rcv);
        }); }
    function make_prototype(name) {
        var prop_names = Array.prototype.slice.call(arguments, 1);
        var param_names = prop_names.join(",");
        var param_inits = prop_names.map(function(prop_name) {
            return "this." + prop_name + "=" + prop_name + ";"; }).join("");
        return eval("(function " + name + "(" + param_names + "){" + param_inits + "})"); }
    function jsnew(ctor) {
        var factoryFunction = constructor.bind.apply(ctor, arguments);
        return new factoryFunction(); }
    function js_function(cmb) {
        return function() {
            var args = cons(this, array_to_list(Array.prototype.slice.call(arguments)));
            return combine(null, null, null, cmb, args);
        } }
    // Apply needs custom implementation to be able to apply JS functions transparently
    function Apply() {}; Apply.prototype.toString = function() { return "apply"; };
    Apply.prototype.wat_combine = function(e, k, f, o) {
        var cmb = elt(o, 0); if (isCapture(cmb)) return cmb;
        var args = elt(o, 1); if (isCapture(args)) return args;
        if (cmb && cmb.wat_combine) return unwrap(cmb).wat_combine(e, k, f, args);
        else if (cmb instanceof Function) return cmb.apply(null, list_to_array(args));
        else return error("apply: not a combiner: " + to_string(cmb)); }
    /* Primitives */
    var primitives =
        ["begin",

         // Primitives

         // Fexprs
         ["def", "--vau", new __Vau()],
         ["def", "eval", wrap(new Eval())],
         ["def", "make-environment", jswrap(function(env) { return make_env(env); })],
         ["def", "wrap", jswrap(wrap)],
         ["def", "unwrap", jswrap(unwrap)],
         ["def", "--set!", new __Set()],
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
         ["def", "--dlet", new __DLet()],
         ["def", "dref", wrap(new DRef())],
         // Errors
         ["def", "--get-root-prompt", jswrap(function() { return ROOT_PROMPT; })],
         ["def", "error", jswrap(error)],
         // JS Interface
         ["def", "js-wrap", jswrap(jswrap)],
         ["def", "js-unop", jswrap(js_unop)],
         ["def", "js-binop", jswrap(js_binop)],
         ["def", "js-getter", jswrap(js_getter)],
         ["def", "js-setter", jswrap(js_setter)],
         ["def", "js-invoker", jswrap(js_invoker)],
         ["def", "js-function", jswrap(js_function)],
         ["def", "js-global", jswrap(function(name) { return global[name]; })],
         ["def", "list-to-array", jswrap(list_to_array)],
         ["def", "array-to-list", jswrap(array_to_list)],
         ["def", "apply", wrap(new Apply())],
         ["def", "--make-object", jswrap(function() { return {}; })],
         ["def", "--make-prototype", jswrap(make_prototype)],
         ["def", "new", jswrap(jsnew)],
         ["def", "xform-typed-lambda", jswrap(xform_typed_lambda)],
         ["def", "--type-check", jswrap(type_check)],
         // Optimization
         ["def", "list*", jswrap(list_star)],

         stdlib.main

        ];
    /* Init */
    var environment = make_env();
    bind(environment, sym("def"), new Def());
    bind(environment, sym("begin"), new Begin());
    evaluate(environment, null, null, parse_json_value(primitives));
    /* API */
    function parse_and_eval(sexp) {
        return eval_expr(parse_json_value(parser.parse_sexp(sexp))); }
    function eval_expr(x) {
        var res = evaluate(environment, null, null, push_root_prompt(x));
        if (isCapture(res)) throw "prompt not found: " + res.prompt;
        return res; }
    function call(fun_name) {
        return eval_expr(parse_json_value([fun_name].concat(Array.prototype.slice.call(arguments, 1)))); }
    return { "eval": parse_and_eval, "call": call };
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./build/stdlib.js":1}]},{},[2])
(2)
});