var wat = (function() {
    /***** Evaluation *****/
    /* Fibers */
    function Fbr() { this.suspensions = null; this.resuming = false; this.val = null; }
    function resume(fbr) {
        fbr.resuming = true;
        var susp = fbr.suspensions.thunk;
        fbr.suspensions = fbr.suspensions.next;
        var res = susp();
        fbr.resuming = fbr.suspensions !== null;
        return res;
    }
    function Suspend() {}
    var SUSPEND = new Suspend();
    function Suspension(thunk, next) { this.thunk = thunk; this.next = next; }
    function pushSuspend(fbr, thunk) { fbr.suspensions = new Suspension(thunk, fbr.suspensions); }
    function evaluate(fbr, e, x) {
        if (x && x.wat_eval) return x.wat_eval(fbr, e); else return x;
    }
    Sym.prototype.wat_eval = function(fbr, e) { return lookup(e, this); };
    Cons.prototype.wat_eval = function(fbr, e) {
        if (fbr.resuming === true) {
            var op = resume(fbr);
        } else {
            var op = evaluate(fbr, e, car(this));
        }
        if (op === SUSPEND) {
            var that = this;
            pushSuspend(fbr, function() { return that.wat_eval(fbr, e); });
            return SUSPEND;
        }
        return combine(fbr, e, op, cdr(this));
    };
    function Def() {}
    Def.prototype.combine = function(fbr, e, o) {
        if (fbr.resuming === true) {
            var val = resume(fbr);
        } else {
            var val = evaluate(fbr, e, elt(o, 1));
        }
        if (val === SUSPEND) {
            pushSuspend(fbr, function() { return Def.prototype.combine(fbr, e, o); });
            return SUSPEND;
        }
        return bind(e, elt(o, 0), val);
    };
    /* Operative & Applicative Combiners */
    function combine(fbr, e, cmb, o) { return cmb.combine ? cmb.combine(fbr, e, o) : fail("not a combiner"); }
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }; function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; }
    Opv.prototype.combine = function(fbr, e, o) {
	var xe = new Env(this.e); bind(xe, this.p, o); bind(xe, this.ep, e);
        return evaluate(fbr, xe, this.x);
    };
    Apv.prototype.combine = function(fbr, e, o) {
        if (fbr.resuming === true) {
            var args = resume(fbr);
        } else {
            var args = evalArgs(fbr, e, o, NIL);
        }
        if (args === SUSPEND) {
            var that = this;
            pushSuspend(fbr, function() { return that.combine(fbr, e, o); })
            return SUSPEND;
        }
        return this.cmb.combine(fbr, e, args);
    };
    function evalArgs(fbr, e, todo, done) {
	if (todo === NIL) { return reverse_list(done); }
        if (fbr.resuming === true) {
            var arg = resume(fbr);
        } else {
            var arg = evaluate(fbr, e, car(todo));
        }
        if (arg === SUSPEND) {
            pushSuspend(fbr, function() { return evalArgs(fbr, e, todo, done); });
            return SUSPEND;
        }
        return evalArgs(fbr, e, cdr(todo), cons(arg, done));
    };
    /* Built-in Combiners */
    function Vau() {}; function If() {}; function Eval() {}; function Begin() {}; function Loop() {};
    function Catch() {}; function Throw() {}; function Finally() {};
    Vau.prototype.combine = function(fbr, e, o) { return new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e); };
    If.prototype.combine = function(fbr, e, o) {
        if (fbr.resuming === true) {
            var test = resume(fbr);
        } else {
            var test = evaluate(fbr, e, elt(o, 0));
        }
        if (test === SUSPEND) {
            pushSuspend(fbr, function() { return If.prototype.combine(fbr, e, o); });
            return SUSPEND;
        }
        return evaluate(fbr, e, (test === F) ? elt(o, 2) : elt(o, 1));
    };
    Eval.prototype.combine = function(fbr, e, o) { return evaluate(fbr, elt(o, 1), elt(o, 0)); };
    Begin.prototype.combine = function(fbr, e, o) { if (o === NIL) return VOID; else return begin(fbr, e, o); };
    function begin(fbr, e, xs) {
        if (fbr.resuming === true) {
            var res = resume(fbr);
        } else {
            var res = evaluate(fbr, e, car(xs));
        }
        if (res === SUSPEND) {
            pushSuspend(fbr, function() { return begin(fbr, e, xs); });
            return SUSPEND;
        }
        var kdr = cdr(xs);
        if (kdr === NIL) return res; else return begin(fbr, e, kdr);
    }
    Loop.prototype.combine = function(fbr, e, o) {
        while (true) {
            if (fbr.resuming === true) {
                var res = resume(fbr);
            } else {
                var res = evaluate(fbr, e, elt(o, 0));
            }
            if (res === SUSPEND) {
                pushSuspend(fbr, function() { return Loop.prototype.combine(fbr, e, o); });
                return SUSPEND;
            }
        }
    }
    Catch.prototype.combine = function(fbr, e, o) {
        var tag = elt(o, 0);
        var th = elt(o, 1);
        try {
            if (fbr.resuming === true) {
                var res = resume(fbr);
            } else {
                var res = combine(fbr, e, th, NIL);
            }
        } catch(exc) {
            if (exc.wat_tag && exc.wat_tag === tag) {
                return exc.wat_val;
            } else {
                throw exc;
            }
        }
        if (res === SUSPEND) {
            pushSuspend(fbr, function() { return Catch.prototype.combine(fbr, e, o); });
            return SUSPEND;
        } else {
            return res;
        }
    };
    function Exc(tag, val) { this.wat_tag = tag; this.wat_val = val; }
    Throw.prototype.combine = function(fbr, e, o) {
        var tag = elt(o, 0);
        var val = elt(o, 1);
        throw new Exc(tag, val);
    };
    Finally.prototype.combine = function(fbr, e, o) {
        var prot = elt(o, 0);
        var cleanup = elt(o, 1);
        try {
            if (fbr.resuming === true) {
                var res = resume(fbr);
            } else {
                var res = evaluate(fbr, e, prot);
            }
            if (res === SUSPEND) {
                pushSuspend(fbr, function() { return Finally.prototype.combine(fbr, e, o); });
            }
        } finally {
            if (res === SUSPEND) {
                return SUSPEND;
            } else {
                return doFinally(fbr, e, cleanup, res);
            }
        }
    };
    function doFinally(fbr, e, cleanup, res) {
        if (fbr.resuming === true) {
            var fres = resume(fbr);
        } else {
            var fres = evaluate(fbr, e, cleanup);
        }
        if (fres === SUSPEND) {
            pushSuspend(fbr, function() { return doFinally(fbr, e, cleanup, res); });
            return SUSPEND;
        } else {
            return res;
        }
    }
    /* Dynamic Variables */
    function DV(val) { this.val = val; }
    function DNew() {}; function DLet() {}; function DRef() {};
    DNew.prototype.combine = function(fbr, e, o) {
        return new DV(elt(o, 0));
    }
    DLet.prototype.combine = function(fbr, e, o) {
        var dv = elt(o, 0);
        var val = elt(o, 1);
        var th = elt(o, 2);
        var oldVal = dv.val;
        dv.val = val;
        try {
            if (fbr.resuming === true) {
                var res = resume(fbr);
            } else {
                var res = combine(fbr, e, th, NIL);
            }
            if (res === SUSPEND) {
                pushSuspend(fbr, function() { return DLet.prototype.combine(fbr, e, o); });
                return SUSPEND;
            } else {
                return res;
            }
        } finally {
            dv.val = oldVal;
        }
    }
    DRef.prototype.combine = function(fbr, e, o) {
        return elt(o, 0).val;
    };
    /* Coroutines */
    function CoroCreate() {}; function CoroResume() {}; function CoroYield() {}; function CurrentCoro() {};
    CoroCreate.prototype.combine = function(fbr, e, o) {
        var cmb = elt(o, 0);
        var newfbr = new Fbr();
        pushSuspend(newfbr, function() { newfbr.resuming = false; return combine(newfbr, e, cmb, NIL); });
        return newfbr;
    };
    CoroResume.prototype.combine = function(fbr, e, o) {
        var newfbr = elt(o, 0);
        var arg = elt(o, 1);
        newfbr.val = arg;
        var res = resume(newfbr);
        if (res === SUSPEND) {
            var val = newfbr.val;
            newfbr.val = null;
            return val;
        } else {
            return res;
        }
    };
    CoroYield.prototype.combine = function(fbr, e, o) {
        var arg = elt(o, 0);
        fbr.val = arg;
        pushSuspend(fbr, function() { return fbr.val; });
        return SUSPEND;
    };
    CurrentCoro.prototype.combine = function(fbr, e, o) {
        return fbr;
    };
    /* JS Bridge */
    function JSFun(jsfun) { this.jsfun = jsfun; }
    JSFun.prototype.combine = function(fbr, e, o) { return this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    var JSOBJ = new Type(); JSOBJ.wat_label = "JS-Object";
    function js_global(name) { return js_prop(WAT_GLOBAL, name); }
    function js_set_global(name, val) { return js_set_prop(WAT_GLOBAL, name, val); }
    function js_prop(obj, name) { assert(type_of(name) === Str.prototype.wat_type); return obj[name.jsstr]; }
    function js_set_prop(obj, name, val) { assert(type_of(name) === Str.prototype.wat_type); return obj[name.jsstr] = val; }
    function js_function(jsfun) { return jswrap(jsfun); }
    function js_method(name) { return jswrap(function(obj) {
	var args = Array.prototype.slice.call(arguments, 1); return obj[name.jsstr].apply(obj, args); }); }
    function to_js(obj) { return (obj && obj.to_js) ? obj.to_js() : obj; }
    Str.prototype.to_js = function() { return this.jsstr; }
    Num.prototype.to_js = function() { return this.jsnum; }
    Bool.prototype.to_js = function() { return this === T ? true : false; }
    function from_js(obj) {
	switch(typeof(obj)) {
	case "string": return new Str(obj);
	case "number": return new Num(obj);
	case "boolean": return obj === true ? T : F;
	default: return obj;
	}
    }
    function JSCallback() {};
    JSCallback.prototype.combine = function(fbr, e, o) {
        var cmb = elt(o, 0);
        return function() {
            var args = array_to_list(Array.prototype.slice.call(arguments));
            var newfbr = new Fbr(); return combine(newfbr, e, cmb, args);
        };
    }
    /***** Objects *****/
    /* Core */
    function Sym(name) { this.name = name; }
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) { assert(type_of(cons) === Cons.prototype.wat_type); return cons.car; }
    function cdr(cons) { assert(type_of(cons) === Cons.prototype.wat_type); return cons.cdr; }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function lookup(e, sym) { var val = e.bindings[sym.name]; return (val !== undefined) ? val : fail("unbound: " + sym.name); }
    function bind(e, lhs, rhs) { lhs.match(e, rhs); return rhs; }
    Sym.prototype.match = function(e, rhs) { e.bindings[this.name] = rhs; };
    Cons.prototype.match = function(e, rhs) { car(this).match(e, car(rhs)); cdr(this).match(e, cdr(rhs)); };
    Nil.prototype.match = function(e, rhs) { if (rhs !== NIL) fail("NIL expected"); };
    Ign.prototype.match = function(e, rhs) {};
    function bound(sym, e) { return (e.bindings[sym.name] !== undefined); }
    var IDHASH = 0; var IDHASH_MAX = Math.pow(2, 53);
    function idhash(obj) { 
        if (obj.wat_idhash === undefined) {
            if (IDHASH >= IDHASH_MAX) IDHASH = 0;
            obj.wat_idhash = IDHASH;
            IDHASH++; }
        return obj.wat_idhash; }
    /* Data */
    function Str(jsstr) { this.jsstr = jsstr; };
    function str_eql(str1, str2) { return str1.jsstr === str2.jsstr; }
    function str_cat(strings) { return strings.map(function(str) { return str.jsstr; }).join(""); }
    function str_print(str1) { return JSON.stringify(str1.jsstr); }
    function Num(jsnum) { this.jsnum = jsnum; };
    function num_eql(num1, num2) { return num1.jsnum === num2.jsnum; }
    function num_lt(num1, num2) { return num1.jsnum < num2.jsnum; }
    function num_add(num1, num2) { return new Num(num1.jsnum + num2.jsnum); };
    function num_sub(num1, num2) { return new Num(num1.jsnum - num2.jsnum); };
    function num_mul(num1, num2) { return new Num(num1.jsnum * num2.jsnum); };
    function num_div(num1, num2) { return new Num(num1.jsnum / num2.jsnum); };
    function num_mod(num1, num2) { return new Num(num1.jsnum % num2.jsnum); };
    function Vector(elements) { this.elements = elements; }
    function vector_ref(vector, i) { return vector.elements[i]; }
    function vector_set(vector, i, val) { vector.elements[i] = val; return val; }
    function vector_length(vector) { return vector.length; }
    function Void() {}; function Ign() {}; function Nil() {}; function Bool() {}
    var VOID = new Void(); var IGN = new Ign(); var NIL = new Nil(); var T = new Bool(); var F = new Bool()
    function str_to_sym(str) { return new Sym(str.jsstr); }
    function sym_to_str(sym) { return new Str(sym.name); }
    function str_to_num(str) { return new Num(Number(str.jsstr)); }
    function num_to_str(num) { return new Str(String(num.jsnum)); }
    function IdentityHashtable() { this.entries = Object.create(null); }
    function hashtable_put(tbl, k, v) {
        var hash = String(idhash(k));
        var bucket = tbl.entries[hash];
        if (bucket === undefined) {
            bucket = [];
            tbl.entries[hash] = bucket;
        }
        for (var i = 0; i < bucket.length; i++) {
            if (bucket[i][0] === k) {
                bucket[i][1] = v;
                return v;
            }
        }
        bucket.push([k, v]);
        return v;
    }
    function hashtable_get(tbl, k, def) {
        var hash = String(idhash(k));
        var bucket = tbl.entries[hash];
        if (bucket === undefined) {
            return def;
        }
        for (var i = 0; i < bucket.length; i++) {
            if (bucket[i][0] === k)
                return bucket[i][1];
        }
        return def;
    }
    /* Types */
    function Type() {};
    function Tagged(type, val) { this.wat_type = type; this.val = val };
    function type_of(obj) { if (obj && obj.wat_type) return obj.wat_type; else return JSOBJ; }
    function make_type() {
	var type = new Type();
	var tagger = jswrap(function(val) { return new Tagged(type, val); });
	var untagger = jswrap(function(obj) { if (type_of(obj) === type) return obj.val; else fail("wrong type"); });
	return cons(type, cons(tagger, cons(untagger, NIL))); }
    function label(type) { return type.wat_label ? type.wat_label : "[anonymous]"; }
    function set_label(type, name) { type.wat_label = name; }
    function dbg(obj) { return obj.dbg ? obj.dbg : VOID; }
    function init_types(typenames) {
        typenames.map(function (typename) { var type = new Type(); set_label(type, typename);
                                            eval(typename).prototype.wat_type = type; }); }
    init_types(["Fbr", "Opv", "Apv", "Def", "Vau", "If", "Eval", "JSFun",
		"Sym", "Cons", "Env", "Str", "Num", "Vector", "Void", "Ign", "Nil", "Bool", "Type"]);
    /* Utilities */
    function assert(b) { if (!b) fail("assertion failed"); }
    function fail(err) { throw err; }
    function array_to_list(array, end) {
	var c = end ? end : NIL; for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c); return c; }
    function list_to_array(c) {
	var res = []; while(c !== NIL) { res.push(car(c)); c = cdr(c); } return res; }
    function reverse_list(list) {
	var res = NIL; while(list !== NIL) { res = cons(car(list), res); list = cdr(list); } return res; }
    /***** Parser *****/
    function parse(s) { // Returns array of forms
	var res = program_stx(ps(s));
	if (res.remaining.index === s.length) return res.ast; else fail("parse error: " + res.remaining.index); }
    var x_stx = function(input) { return x_stx(input); }; // forward decl.
    var id_special_char = choice("-", "&", "!", ":", "=", ">", "<", "%", "+", "?", "/", "*", "#", "$", "_", "'", ".");
    var id_char = choice(range("a", "z"), range("A", "Z"), range("0", "9"), id_special_char);
    // Kludge: don't allow single dot as id, so as not to conflict with dotted pair stx.
    var id_stx = action(join_action(butnot(repeat1(id_char), "."), ""), function (ast) { return new Sym(ast); });
    var escape_char = choice("\"", "\\");
    var escape_sequence = action(sequence("\\", escape_char), function (ast) { return ast[1]; });
    var string_char = choice(negate(escape_char), escape_sequence);
    var string_stx =
	action(sequence("\"", join_action(repeat0(string_char), ""), "\""), function (ast) { return new Str(ast[1]); });
    var digits = join_action(repeat1(range("0", "9")), "");
    var number_stx = action(sequence(optional(choice("+", "-")), digits, optional(join_action(sequence(".", digits), ""))),
			    function (ast) {
				var sign = ast[0] ? ast[0] : "";
				var integral_digits = ast[1]; 
				var fractional_digits = ast[2] || "";
				return new Num(Number(sign + integral_digits + fractional_digits)); });
    function make_constant_stx(string, constant) { return action(string, function(ast) { return constant; }); }
    var void_stx = make_constant_stx("#void", VOID);
    var ign_stx = make_constant_stx("#ign", IGN);
    var nil_stx = make_constant_stx("()", NIL);
    var t_stx = make_constant_stx("#t", T);
    var f_stx = make_constant_stx("#f", F);
    var dot_stx = action(wsequence(".", x_stx), function (ast) { return ast[1]; });
    var compound_stx = action(wsequence("(", repeat1(x_stx), optional(dot_stx), ")"),
			      function(ast) {
				  var exprs = ast[1];
				  var end = ast[2] ? ast[2] : NIL;
				  return array_to_list(exprs, end); });
    var quote_stx = action(sequence("'", x_stx), function(ast) { return array_to_list([new Sym("quote"), ast[1]]); });
    var line_terminator = choice(ch("\r"), ch("\n"));
    var cmt_stx = action(sequence(";", repeat0(negate(line_terminator)), optional(line_terminator)), nothing_action);
    var whitespace_stx = action(choice(" ", "\n", "\r", "\t"), nothing_action);
    function nothing_action(ast) { return VOID; } // HACK!
    var x_stx = whitespace(choice(void_stx, ign_stx, nil_stx, t_stx, f_stx, number_stx,
				  quote_stx, compound_stx, id_stx, string_stx, cmt_stx));
    var program_stx = whitespace(repeat0(choice(x_stx, whitespace_stx))); // HACK!
    /***** Core Environment *****/
    function envbind(e, name, val) { bind(e, new Sym(name), val); set_label(val, name); }
    function mkenvcore() {
	var e = new Env();
	envbind(e, "def", new Def());
	envbind(e, "if", new If());
	envbind(e, "vau", new Vau());
	envbind(e, "eval", wrap(new Eval()));
	envbind(e, "begin", new Begin());
        envbind(e, "loop", new Loop());
        envbind(e, "catch", wrap(new Catch()));
        envbind(e, "throw", wrap(new Throw()));
	envbind(e, "wrap", jswrap(wrap));
	envbind(e, "unwrap", jswrap(unwrap));
	envbind(e, "eq?", jswrap(function (a, b) { return (a === b) ? T : F }));
	envbind(e, "cons", jswrap(cons));
	envbind(e, "make-environment", jswrap(function (parent) { return new Env(parent); }));
        envbind(e, "bound?", jswrap(function (sym, e) { return (bound(sym, e)) ? T : F }));
	envbind(e, "make-type", jswrap(make_type));
	envbind(e, "type-of", jswrap(type_of));
	envbind(e, "label", jswrap(function(type) { return new Str(label(type)); }));
	envbind(e, "set-label!", jswrap(function(type, name) { set_label(type, name.jsstr); return name; }));
	envbind(e, "debug-info", jswrap(dbg));
	envbind(e, "identity-hash-code", jswrap(function(obj) { return new Num(idhash(obj)); }));
	envbind(e, "display", jswrap(function(str) { console.log(str); return str; }));
	envbind(e, "read-from-string", jswrap(function(str) { return array_to_list(parse(str.jsstr)); }));
	envbind(e, "fail", jswrap(fail));
	envbind(e, "num=", jswrap(function(num1, num2) { return num_eql(num1, num2) ? T : F }));
	envbind(e, "num<", jswrap(function(num1, num2) { return num_lt(num1, num2) ? T : F }));
	envbind(e, "+", jswrap(num_add));
	envbind(e, "-", jswrap(num_sub));
	envbind(e, "*", jswrap(num_mul));
	envbind(e, "/", jswrap(num_div));
	envbind(e, "%", jswrap(num_mod));
        envbind(e, "str=", jswrap(function(str1, str2) { return str_eql(str1, str2) ? T : F }));
        envbind(e, "strcat", jswrap(function() {
            return new Str(str_cat.call(null, Array.prototype.slice.call(arguments))); }));
        envbind(e, "str-print", jswrap(function(str) { return new Str(str_print(str)); }));
	envbind(e, "string->symbol", jswrap(str_to_sym));
	envbind(e, "symbol->string", jswrap(sym_to_str));
	envbind(e, "string->number", jswrap(str_to_num));
	envbind(e, "number->string", jswrap(num_to_str));
	envbind(e, "vector", jswrap(function() { return new Vector(Array.prototype.slice.call(arguments)); }));
	envbind(e, "vector-ref", jswrap(function(vector, i) { return vector_ref(vector, i.jsnum); }));
	envbind(e, "vector-set!", jswrap(function(vector, i, val) { return vector_set(vector, i.jsnum, val); }));
	envbind(e, "vector-length", jswrap(function(vector) { return new Num(vector_length(vector)); }));
        envbind(e, "make-identity-hashtable", jswrap(function() { return new IdentityHashtable(); }));
        envbind(e, "hashtable-put!", jswrap(hashtable_put));
        envbind(e, "hashtable-get", jswrap(hashtable_get));
	envbind(e, "js-global", jswrap(js_global));
	envbind(e, "js-set-global!", jswrap(js_set_global));
	envbind(e, "js-prop", jswrap(js_prop));
	envbind(e, "js-set-prop!", jswrap(js_set_prop));
	envbind(e, "js-function", jswrap(js_function));
	envbind(e, "js-method", jswrap(js_method));
	envbind(e, "to-js", jswrap(to_js));
	envbind(e, "from-js", jswrap(from_js));
	envbind(e, "js-callback", wrap(new JSCallback()));
        envbind(e, "coro-create", wrap(new CoroCreate()));
        envbind(e, "coro-resume", wrap(new CoroResume()));
        envbind(e, "coro-yield", wrap(new CoroYield()));
        envbind(e, "current-coro", new CurrentCoro());
        envbind(e, "finally", new Finally());
        envbind(e, "dnew", wrap(new DNew()));
        envbind(e, "dlet*", wrap(new DLet()));
        envbind(e, "dref", wrap(new DRef()));
	return e;
    }
    /***** API *****/
    return {
	"eval": function(x, e) { var fbr = new Fbr(); return evaluate(fbr, e, x); },
	"mkenvcore": mkenvcore, "parse": parse, "Sym": Sym, "array_to_list": array_to_list,
    };
}());
var WAT_GLOBAL = this;
// Abbreviations:
// apv: applicative combiner
// arg: argument
// cmb: combiner
// cmt: comment
// dbg: debugging information
// e: environment
// ep: environment parameter
// fbr: fiber
// id: identifier
// num: number
// o: operand
// opv: operative combiner
// p: parameter
// str: string
// stx: syntax
// sym: symbol
// x: expression
// xe: extended environment
// xs: expressions
