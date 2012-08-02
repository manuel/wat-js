var wat = (function() {
    /***** Evaluation *****/
    function Fbr() { this.a = null; this.k = new KDone(); this.mk = new MKDone(); }
    Fbr.prototype.run = function() { while(this.k !== null) this.k.invoke(this); return this.a; };
    function KDone() {};
    KDone.prototype.invoke = function(fbr) { fbr.mk.underflow(fbr); };
    function MKDone() {};
    MKDone.prototype.underflow = function(fbr) { fbr.k = null; };
    function MKSeg(mk, k) { assert(type_of(mk)); assert(type_of(k)); this.mk = mk; this.k = k; }
    MKSeg.prototype.underflow = function(fbr) { fbr.mk = this.mk; fbr.k = this.k; };
    function MKPrompt(mk, p) { assert(type_of(mk)); assert(type_of(p)); this.mk = mk; this.p = p; }
    MKPrompt.prototype.underflow = function(fbr) { fbr.mk = this.mk; fbr.mk.underflow(fbr); };
    function PushPrompt() {}; function TakeSubCont() {}; function PushSubCont() {}
    PushPrompt.prototype.combine = function(fbr, e, o) {
	var p = elt(o, 0); var th = elt(o, 1);
	fbr.mk = new MKPrompt(new MKSeg(fbr.mk, fbr.k), p);
	fbr.k = new KDone();
	fbr.prime(cons(th, NIL), e);
    };
    TakeSubCont.prototype.combine = function(fbr, e, o) {
	var p = elt(o, 0); var f = elt(o, 1);
	var split = fbr.mk.split_mk(p);
	var sk = new MKSeg(split.sk, fbr.k);
	fbr.mk = split.mk;
	fbr.k = new KDone();
	fbr.prime(cons(f, cons(sk, NIL)), e);
    };
    PushSubCont.prototype.combine = function(fbr, e, o) {
	var sk = elt(o, 0); var th = elt(o, 1);
	fbr.mk = sk.append_mk(new MKSeg(fbr.mk, fbr.k));
	fbr.k = new KDone();
	fbr.prime(cons(th, NIL), e);
    };
    Fbr.prototype.prime = function(x, e) { this.a = x; this.k = new KEval(this.k, e); };
    function KEval(k, e) { this.k = k; this.e = e; }
    KEval.prototype.invoke = function(fbr) { fbr.a.evaluate ? fbr.a.evaluate(fbr, this.k, this.e) : fbr.k = this.k; };
    Sym.prototype.evaluate = function(fbr, k, e) { fbr.k = k; fbr.a = lookup(e, this); };
    Cons.prototype.evaluate = function(fbr, k, e) { fbr.k = new KCombine(k, e, cdr(this)); fbr.prime(car(this), e); };
    function KCombine(k, e, o) { this.k = k; this.e = e; this.o = o; }
    KCombine.prototype.invoke = function(fbr) { fbr.k = this.k; fbr.a.combine(fbr, this.e, this.o); };
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }; function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; }
    function Def() {}; function Vau() {}; function If() {}; function Eval() {}
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
    Def.prototype.combine = function(fbr, e, o) { fbr.k = new KDef(fbr.k, e, elt(o, 0)); fbr.prime(elt(o, 1), e); };
    function KDef(k, e, name) { this.k = k; this.e = e; this.name = name; }
    KDef.prototype.invoke = function(fbr) { fbr.k = this.k; bind(this.e, this.name, fbr.a); }
    Vau.prototype.combine = function(fbr, e, o) { fbr.a = new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e); };
    If.prototype.combine = function(fbr, e, o) { fbr.k = new KIf(fbr.k, e, elt(o, 1), elt(o, 2)); fbr.prime(elt(o, 0), e); };
    function KIf(k, e, xthen, xelse) { this.k = k; this.e = e; this.xthen = xthen; this.xelse = xelse; }
    KIf.prototype.invoke = function(fbr) { fbr.k = this.k; fbr.prime(fbr.a === F ? this.xelse : this.xthen, this.e); };
    Eval.prototype.combine = function(fbr, e, o) { fbr.prime(elt(o, 0), elt(o, 1)); };
    function CallWithMark() {}; function CurrentMarks() {}
    CallWithMark.prototype.combine = function(fbr, e, o) {
	var key = elt(o, 0); var val = elt(o, 1); var th = elt(o, 2);
	fbr.k = clone(fbr.k); fbr.k["m_" + key.name] = val; fbr.prime(cons(th, NIL), e); };
    CurrentMarks.prototype.combine = function(fbr, e, o) {
	var key = elt(o, 0); var res = [];
	k_marks(fbr.k, key, res); fbr.mk.mk_marks(key, res); fbr.a = array_to_list(res); };
    function k_marks(k, key, res) {
	while(k) { var val = k["m_" + key.name]; if (val !== undefined) res.push(val); k = k.k; } }
    MKDone.prototype.mk_marks = function(key, res) {};
    MKPrompt.prototype.mk_marks = function(key, res) { this.mk.mk_marks(key, res); };
    MKSeg.prototype.mk_marks = function(key, res) { k_marks(this.k, key, res); this.mk.mk_marks(key, res); };
    function JSFun(jsfun) { this.jsfun = jsfun }
    JSFun.prototype.combine = function(fbr, e, o) { fbr.a = this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    /* Metacontinuation Utilities */
    MKDone.prototype.split_mk = function(p) { fail("prompt not found"); };
    MKPrompt.prototype.split_mk = function(p) {
	if (p === this.p) { return { sk: new MKDone(), mk: this.mk }; }
	else { var split = this.mk.split_mk(p); return { sk: new MKPrompt(split.sk, this.p), mk: split.mk }; } };
    MKSeg.prototype.split_mk = function(p) {
	var split = this.mk.split_mk(p);
	return { sk: new MKSeg(split.sk, this.k), mk: split.mk }; };
    MKDone.prototype.append_mk = function(mk) { return mk; };
    MKPrompt.prototype.append_mk = function(mk) { return new MKPrompt(this.mk.append_mk(mk), this.p); };
    MKSeg.prototype.append_mk = function(mk) { return new MKSeg(this.mk.append_mk(mk), this.k); };
    /***** Data *****/
    function Sym(name) { this.name = name; }
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) { assert(type_of(cons) === Cons.prototype.wat_type); return cons.car; }
    function cdr(cons) { assert(type_of(cons) === Cons.prototype.wat_type); return cons.cdr; }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function lookup(e, sym) { var val = e.bindings[sym.name]; return val ? val : fail("unbound: " + sym.name); }
    function bind(e, lhs, rhs) { lhs.match(e, rhs); }
    Sym.prototype.match = function(e, rhs) { assert(type_of(rhs)); e.bindings[this.name] = rhs; };
    Cons.prototype.match = function(e, rhs) { car(this).match(e, car(rhs)); cdr(this).match(e, cdr(rhs)); };
    Nil.prototype.match = function(e, rhs) { if (rhs !== NIL) fail("NIL expected"); };
    Ign.prototype.match = function(e, rhs) {};
    function Str(jsstr) { this.jsstr = jsstr };
    function Num(jsnum) { this.jsnum = jsnum };
    function num_eql(num1, num2) { return num1.jsnum === num2.jsnum; }
    function num_add(num1, num2) { return new Num(num1.jsnum + num2.jsnum); };
    function Vector(elements) { this.elements = elements; }
    function vector_ref(vector, i) { return vector.elements[i]; }
    function vector_set(vector, i, val) { vector.elements[i] = val; return val; }
    function vector_length(vector) { return vector.length; }
    function Void() {}; function Ign() {}; function Nil() {}; function True() {}; function False() {};
    var VOID = new Void(); var IGN = new Ign(); var NIL = new Nil(); var T = new True(); var F = new False();
    function Type() { this.e = new Env() };
    function type_of(obj) { return obj.wat_type; }
    function type_env(type) { return type.e; };
    function Tagged(type, val) { this.wat_type = type; this.val = val };
    function tag(type, val) { return new Tagged(type, val); };
    function untag(obj) { return obj.val; }
    function init_types(types) { types.map(function (type) { type.prototype.wat_type = new Type(); }); }
    init_types([KDone, KEval, KCombine, KApply, KEvalArg, KDef, KIf, MKDone, MKPrompt, MKSeg,
		Opv, Apv, Def, Vau, If, Eval, CallWithMark, CurrentMarks, JSFun,
		Sym, Cons, Env, Str, Num, Vector, Void, Ign, Nil, True, False, Type]);
    function assert(b) { if (!b) fail("assertion failed"); }
    function fail(err) { throw err; }
    function array_to_list(array, end) {
	var c = end ? end : NIL; for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c); return c; }
    function list_to_array(c) {
	var res = []; while(c !== NIL) { res.push(car(c)); c = cdr(c); } return res; }
    function reverse_list(list) {
	var res = NIL; while(list !== NIL) { res = cons(car(list), res); list = cdr(list); } return res; }
    function clone(obj) {
        var cl = Object.create(obj.constructor.prototype);
	for (var k in obj) if (obj.hasOwnProperty(k)) cl[k] = obj[k]; return cl; };
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
    /* Core Environment */
    function mkenvcore() {
	var e = new Env();
	bind(e, new Sym("def"), new Def());
	bind(e, new Sym("if"), new If());
	bind(e, new Sym("push-prompt*"), wrap(new PushPrompt()));
	bind(e, new Sym("take-sub-cont*"), wrap(new TakeSubCont()));
	bind(e, new Sym("push-sub-cont*"), wrap(new PushSubCont()));
	bind(e, new Sym("call-with-mark"), wrap(new CallWithMark()));
	bind(e, new Sym("current-marks"), wrap(new CurrentMarks()));
	bind(e, new Sym("vau"), new Vau());
	bind(e, new Sym("eval"), wrap(new Eval()));
	bind(e, new Sym("wrap"), jswrap(wrap));
	bind(e, new Sym("unwrap"), jswrap(unwrap));
	bind(e, new Sym("eq?"), jswrap(function (a, b) { return (a === b) ? T : F }));
	bind(e, new Sym("cons"), jswrap(cons));
	bind(e, new Sym("make-environment"), jswrap(function (parent) { return new Env(parent); }));
	bind(e, new Sym("make-type"), jswrap(function () { return new Type(); }));
	bind(e, new Sym("type-environment"), jswrap(type_env));
	bind(e, new Sym("type-of"), jswrap(type_of));
	bind(e, new Sym("tag"), jswrap(tag));
	bind(e, new Sym("untag"), jswrap(untag));
	bind(e, new Sym("display"), jswrap(function(str) { console.log(str); return str; }));
	bind(e, new Sym("fail"), jswrap(fail));
	bind(e, new Sym("="), jswrap(function(num1, num2) { return num_eql(num1, num2) ? T : F }));
	bind(e, new Sym("+"), jswrap(num_add));
	bind(e, new Sym("vector"), jswrap(function() { return new Vector(Array.prototype.slice.call(arguments)); }));
	bind(e, new Sym("vector-ref"), jswrap(function(vector, i) { return vector_ref(vector, i.jsnum); }));
	bind(e, new Sym("vector-set!"), jswrap(function(vector, i, val) { return vector_set(vector, i.jsnum, val); }));
	bind(e, new Sym("vector-length"), jswrap(function(vector) { return new Num(vector_length(vector)); }));
	return e;
    }
    /* API */
    return {
	"eval": function(x, e) { var fbr = new Fbr(); fbr.prime(x, e); return fbr.run(); },
	"mkenvcore": mkenvcore, "parse": parse,
    };
}());
