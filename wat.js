var wat = (function() {
    /***** Evaluation *****/
    /* Fibers */
    function Fbr() { this.a = null; this.k = new KDone(); this.mk = new MKDone(); }
    Fbr.prototype.run = function() { while(this.k !== null) this.k.invoke(this); return this.a; };
    Fbr.prototype.prime = function(x, e) { this.a = x; this.k = new KEval(this.k, e); };
    /* Form Evaluation */
    function KEval(k, e) { this.k = k; this.e = e; }
    KEval.prototype.invoke = function(fbr) { (fbr.a && fbr.a.evaluate) ? fbr.a.evaluate(fbr, this.k, this.e) : fbr.k = this.k; };
    Sym.prototype.evaluate = function(fbr, k, e) { fbr.k = k; fbr.a = lookup(e, this); };
    Cons.prototype.evaluate = function(fbr, k, e) { fbr.k = new KCombine(k, e, cdr(this)); fbr.prime(car(this), e); };
    /* Operative & Applicative Combiners */
    function KCombine(k, e, o) { this.k = k; this.e = e; this.o = o; }
    KCombine.prototype.invoke = function(fbr) { fbr.k = this.k; fbr.a.combine(fbr, this.e, this.o); };
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }; function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; }
    Opv.prototype.combine = function(fbr, e, o) {
	var xe = new Env(this.e); bind(xe, this.p, o); bind(xe, this.ep, e); fbr.prime(this.x, xe); };
    Apv.prototype.combine = function(fbr, e, o) { evalArgs(fbr, new KApply(fbr.k, e, this.cmb), e, o, NIL); };
    function KApply(k, e, cmb) { this.k = k; this.e = e; this.cmb = cmb; }
    KApply.prototype.invoke = function(fbr) { fbr.k = this.k; fbr.prime(cons(this.cmb, fbr.a), this.e); };
    function evalArgs(fbr, k, e, todo, done) {
	if (todo === NIL) { fbr.a = reverse_list(done); fbr.k = k; }
	else { fbr.k = new KEvalArg(k, e, cdr(todo), done); fbr.prime(car(todo), e); } }
    function KEvalArg(k, e, todo, done) { this.k = k; this.e = e; this.todo = todo; this.done = done; }
    KEvalArg.prototype.invoke = function(fbr) { evalArgs(fbr, this.k, this.e, this.todo, cons(fbr.a, this.done)); };
    /* Built-in Combiners */
    function Def() {}; function Vau() {}; function If() {}; function Eval() {}; function Begin() {}
    Def.prototype.combine = function(fbr, e, o) { fbr.k = new KDef(fbr.k, e, elt(o, 0)); fbr.prime(elt(o, 1), e); };
    function KDef(k, e, name) { this.k = k; this.e = e; this.name = name; }
    KDef.prototype.invoke = function(fbr) { fbr.k = this.k; bind(this.e, this.name, fbr.a); }
    Vau.prototype.combine = function(fbr, e, o) { fbr.a = new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e); };
    If.prototype.combine = function(fbr, e, o) { fbr.k = new KIf(fbr.k, e, elt(o, 1), elt(o, 2)); fbr.prime(elt(o, 0), e); };
    function KIf(k, e, xthen, xelse) { this.k = k; this.e = e; this.xthen = xthen; this.xelse = xelse; }
    KIf.prototype.invoke = function(fbr) { fbr.k = this.k; fbr.prime(fbr.a === F ? this.xelse : this.xthen, this.e); };
    Eval.prototype.combine = function(fbr, e, o) { fbr.prime(elt(o, 0), elt(o, 1)); };
    Begin.prototype.combine = function(fbr, e, o) { if (o === NIL) fbr.a = VOID; else begin1(fbr, e, o); };
    function begin1(fbr, e, xs) { if (cdr(xs) !== NIL) { fbr.k = new KBegin(fbr.k, e, cdr(xs)); } fbr.prime(car(xs), e); }
    function KBegin(k, e, xs) { this.k = k; this.e = e; this.xs = xs; }
    KBegin.prototype.invoke = function(fbr) { fbr.k = this.k; begin1(fbr, this.e, this.xs); }
    /* Delimited Control and Metacontinuations */
    function KDone() {};
    KDone.prototype.invoke = function(fbr) { fbr.mk.underflow(fbr); };
    function MKDone() {};
    MKDone.prototype.underflow = function(fbr) { fbr.k = null; };
    function MKSeg(mk, k) { assert(type_of(mk)); assert(type_of(k)); this.mk = mk; this.k = k; }
    function mkseg(mk, k) { if (k instanceof KDone) return mk; else return new MKSeg(mk, k); } // TCO
    MKSeg.prototype.underflow = function(fbr) { fbr.mk = this.mk; fbr.k = this.k; };
    function MKPrompt(mk, p) { assert(type_of(mk)); assert(type_of(p)); this.mk = mk; this.p = p; }
    MKPrompt.prototype.underflow = function(fbr) { fbr.mk = this.mk; fbr.mk.underflow(fbr); };
    function PushPrompt() {}; function TakeSubCont() {}; function PushSubCont() {}
    PushPrompt.prototype.combine = function(fbr, e, o) {
	var p = elt(o, 0); var th = elt(o, 1);
	fbr.mk = new MKPrompt(mkseg(fbr.mk, fbr.k), p);
	fbr.k = new KDone();
	fbr.prime(cons(th, NIL), e); };
    TakeSubCont.prototype.combine = function(fbr, e, o) {
	var p = elt(o, 0); var f = elt(o, 1);
	var split = fbr.mk.split_mk(p);
	var sk = mkseg(split.sk, fbr.k);
	fbr.mk = split.mk;
	fbr.k = new KDone();
	fbr.prime(cons(f, cons(sk, NIL)), e); };
    PushSubCont.prototype.combine = function(fbr, e, o) {
	var sk = elt(o, 0); var th = elt(o, 1);
	fbr.mk = sk.append_mk(mkseg(fbr.mk, fbr.k));
	fbr.k = new KDone();
	fbr.prime(cons(th, NIL), e); };
    MKDone.prototype.split_mk = function(p) { fail("prompt not found"); };
    MKPrompt.prototype.split_mk = function(p) {
	if (p === this.p) { return { sk: new MKDone(), mk: this.mk }; }
	else { var split = this.mk.split_mk(p); return { sk: new MKPrompt(split.sk, this.p), mk: split.mk }; } };
    MKSeg.prototype.split_mk = function(p) {
	var split = this.mk.split_mk(p);
	return { sk: mkseg(split.sk, this.k), mk: split.mk }; };
    MKDone.prototype.append_mk = function(mk) { return mk; };
    MKPrompt.prototype.append_mk = function(mk) { return new MKPrompt(this.mk.append_mk(mk), this.p); };
    MKSeg.prototype.append_mk = function(mk) { return mkseg(this.mk.append_mk(mk), this.k); };
    /* JS Bridge */
    function JSFun(jsfun) { this.jsfun = jsfun; }
    JSFun.prototype.combine = function(fbr, e, o) { fbr.a = this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    var JSOBJ = new Type();
    function js_global(name) { return js_prop(WAT_GLOBAL, name); }
    function js_set_global(name, val) { return js_set_prop(WAT_GLOBAL, name, val); }
    function js_prop(obj, name) { assert(type_of(name) === Str.prototype.wat_type); return obj[name.jsstr]; }
    function js_set_prop(obj, name, val) { assert(type_of(name) === Str.prototype.wat_type); return obj[name.jsstr] = val; }
    function js_function(jsfun) { return jswrap(jsfun); }
    function js_method(name) { return jswrap(function(obj) {
	var args = Array.prototype.slice.call(arguments, 1); return obj[name.jsstr].apply(obj, args); }); }
    function JSCallback() {}
    JSCallback.prototype.combine = function(fbr, e, o) {
	fbr.a = function() { var args = array_to_list(Array.prototype.slice.call(arguments));
			     fbr.prime(cons(elt(o, 0), args), e); fbr.run(); }; };
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
    function bind(e, lhs, rhs) { lhs.match(e, rhs); }
    Sym.prototype.match = function(e, rhs) { assert(type_of(rhs)); e.bindings[this.name] = rhs; };
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
    function Num(jsnum) { this.jsnum = jsnum; };
    function num_eql(num1, num2) { return num1.jsnum === num2.jsnum; }
    function num_add(num1, num2) { return new Num(num1.jsnum + num2.jsnum); };
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
    /* Types */
    function Type() {};
    function Tagged(type, val) { this.wat_type = type; this.val = val };
    function type_of(obj) { if (obj && obj.wat_type) return obj.wat_type; else return JSOBJ; }
    function make_type() {
	var type = new Type();
	var tagger = jswrap(function(val) { return new Tagged(type, val); });
	var untagger = jswrap(function(obj) { if (type_of(obj) === type) return obj.val; else fail("wrong type"); });
	return cons(type, cons(tagger, cons(untagger, NIL))); }
    function init_types(types) { types.map(function (type) { type.prototype.wat_type = new Type(); }); }
    init_types([KDone, KEval, KCombine, KApply, KEvalArg, KDef, KIf, KBegin, MKDone, MKPrompt, MKSeg,
		Opv, Apv, Def, Vau, If, Eval, Begin, JSFun, JSCallback,
		Sym, Cons, Env, Str, Num, Vector, Void, Ign, Nil, Bool, Type]);
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
    function parse(s) {
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
    function mkenvcore() {
	var e = new Env();
	bind(e, new Sym("def"), new Def());
	bind(e, new Sym("if"), new If());
	bind(e, new Sym("push-prompt*"), wrap(new PushPrompt()));
	bind(e, new Sym("take-sub-cont*"), wrap(new TakeSubCont()));
	bind(e, new Sym("push-sub-cont*"), wrap(new PushSubCont()));
	bind(e, new Sym("vau"), new Vau());
	bind(e, new Sym("eval"), wrap(new Eval()));
	bind(e, new Sym("begin"), new Begin());
	bind(e, new Sym("wrap"), jswrap(wrap));
	bind(e, new Sym("unwrap"), jswrap(unwrap));
	bind(e, new Sym("eq?"), jswrap(function (a, b) { return (a === b) ? T : F }));
	bind(e, new Sym("cons"), jswrap(cons));
	bind(e, new Sym("make-environment"), jswrap(function (parent) { return new Env(parent); }));
        bind(e, new Sym("bound?"), jswrap(function (sym, e) { return (bound(sym, e)) ? T : F }));
	bind(e, new Sym("make-type"), jswrap(make_type));
	bind(e, new Sym("type-of"), jswrap(type_of));
	bind(e, new Sym("idhash"), jswrap(function(obj) { return new Num(idhash(obj)); }));
	bind(e, new Sym("display"), jswrap(function(str) { console.log(str); return str; }));
	bind(e, new Sym("read-from-string"), jswrap(function(str) { return array_to_list(parse(str.jsstr)); }));
	bind(e, new Sym("fail"), jswrap(fail));
	bind(e, new Sym("="), jswrap(function(num1, num2) { return num_eql(num1, num2) ? T : F }));
	bind(e, new Sym("+"), jswrap(num_add));
	bind(e, new Sym("string->symbol"), jswrap(str_to_sym));
	bind(e, new Sym("symbol->string"), jswrap(sym_to_str));
	bind(e, new Sym("string->number"), jswrap(str_to_num));
	bind(e, new Sym("number->string"), jswrap(num_to_str));
	bind(e, new Sym("vector"), jswrap(function() { return new Vector(Array.prototype.slice.call(arguments)); }));
	bind(e, new Sym("vector-ref"), jswrap(function(vector, i) { return vector_ref(vector, i.jsnum); }));
	bind(e, new Sym("vector-set!"), jswrap(function(vector, i, val) { return vector_set(vector, i.jsnum, val); }));
	bind(e, new Sym("vector-length"), jswrap(function(vector) { return new Num(vector_length(vector)); }));
	bind(e, new Sym("js-global"), jswrap(js_global));
	bind(e, new Sym("js-set-global!"), jswrap(js_set_global));
	bind(e, new Sym("js-prop"), jswrap(js_prop));
	bind(e, new Sym("js-set-prop!"), jswrap(js_set_prop));
	bind(e, new Sym("js-function"), jswrap(js_function));
	bind(e, new Sym("js-method"), jswrap(js_method));
	bind(e, new Sym("js-callback"), wrap(new JSCallback()));
	bind(e, new Sym("to-js"), jswrap(to_js));
	bind(e, new Sym("from-js"), jswrap(from_js));
	return e;
    }
    /***** API *****/
    return {
	"eval": function(x, e) { var fbr = new Fbr(); fbr.prime(x, e); return fbr.run(); },
	"mkenvcore": mkenvcore, "parse": parse,
    };
}());
var WAT_GLOBAL = this;
// Abbreviations:
// a: accumulator
// apv: applicative combiner
// arg: argument
// cmb: combiner
// cmt: comment
// e: environment
// ep: environment parameter
// fbr: fiber
// id: identifier
// k: continuation
// mk: metacontinuation
// num: number
// o: operand
// opv: operative combiner
// p: parameter
// seg: metacontinuation segment
// str: string
// stx: syntax
// subcont: subcontinuation
// sym: symbol
// x: expression
// xe: extended environment
// xs: expressions
