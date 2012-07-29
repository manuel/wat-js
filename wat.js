var wat = (function() {
    /***** Evaluation *****/
    function Fbr() { this.a = null; this.k = null; }
    Fbr.prototype.run = function() { while(this.k) this.k.invoke(this); return this.a; };
    Fbr.prototype.prime = function(x, e) { this.a = x; this.k = new KEval(this.k, e); };
    function KEval(k, e) { this.k = k; this.e = e; }
    KEval.prototype.invoke = function(fbr) { fbr.a.evaluate ? fbr.a.evaluate(fbr, this.k, this.e) : fbr.k = this.k; };
    Sym.prototype.evaluate = function(fbr, k, e) { fbr.k = k; fbr.a = lookup(e, this); };
    Cons.prototype.evaluate = function(fbr, k, e) { fbr.k = new KCombine(k, e, cdr(this)); fbr.prime(car(this), e); };
    function KCombine(k, e, o) { this.k = k; this.e = e; this.o = o; }
    KCombine.prototype.invoke = function(fbr) { fbr.k = this.k; fbr.a.combine(fbr, this.e, this.o); };
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }; function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; }
    function Def() {}; function Vau() {}; function If() {}; function Eval() {}; function CCC() {}; function Jump() {}
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
    CCC.prototype.combine = function(fbr, e, o) { fbr.a = cons(fbr.k, NIL); fbr.k = new KApply(fbr.k, e, elt(o, 0)); };
    Jump.prototype.combine = function(fbr, e, o) { fbr.k = elt(o, 0); fbr.a = elt(o, 1); };
    function JSFun(jsfun) { this.jsfun = jsfun }
    JSFun.prototype.combine = function(fbr, e, o) { fbr.a = this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    /***** Data *****/
    function Sym(name) { this.name = name; }
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) { return cons.car; }
    function cdr(cons) { return cons.cdr; }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function lookup(e, sym) { var val = e.bindings[sym.name]; return val ? val : fail("unbound: " + sym.name); }
    function bind(e, lhs, rhs) { lhs.match(e, rhs); }
    Sym.prototype.match = function(e, rhs) { e.bindings[this.name] = rhs; }
    Cons.prototype.match = function(e, rhs) { car(this).match(e, car(rhs)); cdr(this).match(e, cdr(rhs)); }
    Nil.prototype.match = function(e, rhs) { if (rhs !== NIL) fail("NIL expected"); }
    Ign.prototype.match = function(e, rhs) {}
    function Str(jsstr) { this.jsstr = jsstr };
    function Num(jsnum) { this.jsnum = jsnum };
    function Void() {}; function Ign() {}; function Nil() {}; function True() {}; function False() {};
    var VOID = new Void(); var IGN = new Ign(); var NIL = new Nil(); var T = new True(); var F = new False();
    function Type() { this.e = new Env() };
    function type_of(obj) { return obj.wat_type; }
    function type_env(type) { return type.e; };
    function Tagged(type, val) { this.wat_type = type; this.val = val };
    function tag(type, val) { return new Tagged(type, val); };
    function untag(obj) { return obj.val; }
    function init_types(types) { types.map(function (type) { type.prototype.wat_type = new Type(); }); }
    init_types([Opv, Apv, Def, Vau, If, Eval, CCC, Jump, JSFun, Sym, Cons, Env, Str, Num, Void, Ign, Nil, True, False, Type]);
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
    var string_stx = action(sequence("\"", join_action(repeat0(string_char), ""), "\""),
			    function (ast) { return new Str(ast[1]); });
    var digits = join_action(repeat1(range("0", "9")), "");
    var number_stx = action(sequence(optional(choice("+", "-")), digits, optional(join_action(sequence(".", digits), ""))),
			    function number_act() {
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
			      function (ast) {
				  var exprs = ast[1];
				  var end = ast[2] ? ast[2] : NIL;
				  return array_to_list(exprs, end); });
    var line_terminator = choice(ch("\r"), ch("\n"));
    var cmt_stx = action(sequence(";", repeat0(negate(line_terminator)), optional(line_terminator)), nothing_action);
    var whitespace_stx = action(choice(" ", "\n", "\r", "\t"), nothing_action);
    function nothing_action(ast) { return VOID; } // HACK!
    var x_stx =
	whitespace(choice(void_stx, ign_stx, nil_stx, t_stx, f_stx, number_stx, compound_stx, id_stx, string_stx, cmt_stx));
    var program_stx = whitespace(repeat0(choice(x_stx, whitespace_stx))); // HACK!
    /* Core Environment */
    function mkenvcore() {
	var e = new Env();
	bind(e, new Sym("def"), new Def());
	bind(e, new Sym("if"), new If());
	bind(e, new Sym("ccc"), wrap(new CCC()));
	bind(e, new Sym("jump"), wrap(new Jump()));
	bind(e, new Sym("vau"), new Vau());
	bind(e, new Sym("eval"), wrap(new Eval()));
	bind(e, new Sym("wrap"), jswrap(wrap));
	bind(e, new Sym("unwrap"), jswrap(unwrap));
	bind(e, new Sym("eq"), jswrap(function (a, b) { return (a === b) ? T : F }));
	bind(e, new Sym("cons"), jswrap(cons));
	bind(e, new Sym("make-environment"), jswrap(function (parent) { return new Env(parent); }));
	bind(e, new Sym("make-type"), jswrap(function () { return new Type(); }));
	bind(e, new Sym("type-environment"), jswrap(type_env));
	bind(e, new Sym("type-of"), jswrap(type_of));
	bind(e, new Sym("tag"), jswrap(tag));
	bind(e, new Sym("untag"), jswrap(untag));
	bind(e, new Sym("fail"), jswrap(fail));
	return e;
    }
    /* API */
    return {
	"eval": function(x, e) { var fbr = new Fbr(); fbr.prime(x, e); return fbr.run(); },
	"mkenvcore": mkenvcore, "parse": parse,
    };
}());
