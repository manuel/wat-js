var wat = (function() {
    /***** Evaluation *****/
    /* Fibers */
    // function Fbr(e) { this.e = e; this.a = null; this.st = [null]; this.ip = 0; }
    // function run(fbr) { while(fbr.st[fbr.ip] !== null) fbr.st[fbr.ip].invoke(fbr); return fbr.a; };
    // function acc(fbr, x) { assert(x !== undefined); fbr.a = x; }
    // function push(fbr, v) { fbr.st.push(v); fbr.ip++; }
    // function idx(fbr, i) { return fbr.st[fbr.ip - i]; }
    // function ret(fbr) { var sz = fbr.st[fbr.ip].sz; for(var i = 0; i < sz; i++) { fbr.st.pop(); fbr.ip--; } }
    // function prime(fbr, e, x) { acc(fbr, x); pushEval(fbr, e); }
    function Fbr(e) { this.e = e; this.a = null; this.st = null; }
    function run(fbr) { while(fbr.st !== null) fbr.st.v.invoke(fbr); return fbr.a; };
    function acc(fbr, x) { assert(x !== undefined); fbr.a = x; }
    function push(fbr, v) { fbr.st = { next: fbr.st, v: v }; }
    function idx(fbr, i) { var st = fbr.st; while(i > 0) { st = st.next; i--; } return st.v; }
    function ret(fbr) { var sz = fbr.st.v.sz; for(var i = 0; i < sz; i++) fbr.st = fbr.st.next; }
    function prime(fbr, e, x) { acc(fbr, x); pushEval(fbr, e); }
    /* Form Evaluation */
    function pushEval(fbr, e) { push(fbr, e); push(fbr, KEVAL); }
    function KEval() {}; KEval.prototype.sz = 2; KEval.prototype.label = "KEVAL"; var KEVAL = new KEval();
    KEval.prototype.invoke = function(fbr) { if(fbr.a && fbr.a.evaluate) fbr.a.evaluate(fbr, idx(fbr, 1)); else ret(fbr); }
    Sym.prototype.evaluate = function(fbr, e) { ret(fbr); acc(fbr, lookup(e, this)); }
    Cons.prototype.evaluate = function(fbr, e) { ret(fbr); pushCombine(fbr, e, cdr(this)); prime(fbr, e, car(this)); }
    /* Operative & Applicative Combiners */
    function pushCombine(fbr, e, o) { push(fbr, o); push(fbr, e); push(fbr, KCOMBINE); }
    function KCombine() {}; KCombine.prototype.sz = 3; KCombine.prototype.label = "KCOMBINE"; var KCOMBINE = new KCombine();
    KCombine.prototype.invoke = function(fbr) {
        if(fbr.a && fbr.a.combine) { var e = idx(fbr, 1); var o = idx(fbr, 2); ret(fbr); fbr.a.combine(fbr, e, o); }
        else fail("not a combiner", fbr.a); }
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }; function wrap(cmb) { return new Apv(cmb); }; function unwrap(apv) { return apv.cmb; };
    Opv.prototype.combine = function(fbr, e, o) {
	var xe = new Env(this.e); bind(xe, this.p, o); bind(xe, this.ep, e); prime(fbr, xe, this.x); };
    Apv.prototype.combine = function(fbr, e, o) { pushApply(fbr, e, this.cmb); evalArgs(fbr, e, o, NIL); }
    function pushApply(fbr, e, cmb) { push(fbr, cmb); push(fbr, e); push(fbr, KAPPLY); }
    function KApply() {}; KApply.prototype.sz = 3; KApply.prototype.label = "KAPPLY"; var KAPPLY = new KApply();
    KApply.prototype.invoke = function(fbr) {
        var e = idx(fbr, 1); var cmb = idx(fbr, 2); ret(fbr); prime(fbr, e, cons(cmb, fbr.a)); };
    function evalArgs(fbr, e, todo, done) {
        if (todo === NIL) { acc(fbr, reverse_list(done)); }
        else { pushEvalArg(fbr, e, cdr(todo), done); prime(fbr, e, car(todo)); } }
    function pushEvalArg(fbr, e, todo, done) { push(fbr, done); push(fbr, todo); push(fbr, e); push(fbr, KEVALARG); }
    function KEvalArg() {}; KEvalArg.prototype.sz = 4; KEvalArg.prototype.label = "KEVALARG"; var KEVALARG = new KEvalArg();
    KEvalArg.prototype.invoke = function(fbr) {
        var e = idx(fbr, 1); var todo = idx(fbr, 2); var done = idx(fbr, 3);
        ret(fbr); evalArgs(fbr, e, todo, cons(fbr.a, done)); };
    /* Built-in Combiners */
    function Def() {}; function Vau() {}; function If() {}; function Eval() {}; function Begin() {};
    Def.prototype.combine = function(fbr, e, o) { pushDef(fbr, e, elt(o, 0)); prime(fbr, e, elt(o, 1)); };
    function pushDef(fbr, e, name) { push(fbr, name); push(fbr, e); push(fbr, KDEF); }
    function KDef() {}; KDef.prototype.sz = 3; KDef.prototype.label = "KDEF"; var KDEF = new KDef();
    KDef.prototype.invoke = function(fbr) { var e = idx(fbr, 1); var name = idx(fbr, 2); bind(e, name, fbr.a); ret(fbr); };
    Vau.prototype.combine = function(fbr, e, o) { acc(fbr, new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e)); };
    If.prototype.combine = function(fbr, e, o) { pushIf(fbr, e, elt(o, 1), elt(o, 2)); prime(fbr, e, elt(o, 0)); };
    function pushIf(fbr, e, xthen, xelse) { push(fbr, xelse); push(fbr, xthen); push(fbr, e); push(fbr, KIF); }
    function KIf() {}; KIf.prototype.sz = 4; KIf.prototype.label = "KIF"; var KIF = new KIf();
    KIf.prototype.invoke = function(fbr) {
        var e = idx(fbr, 1); var xthen = idx(fbr, 2); var xelse = idx(fbr, 3);
        ret(fbr); prime(fbr, e, fbr.a === F ? xelse : xthen); }
    Eval.prototype.combine = function(fbr, e, o) { prime(fbr, elt(o, 1), elt(o, 0)); };
    Begin.prototype.combine = function(fbr, e, o) { if (o === NIL) acc(fbr, VOID); else begin1(fbr, e, o); };
    function begin1(fbr, e, xs) { if (cdr(xs) !== NIL) pushBegin(fbr, e, cdr(xs)); prime(fbr, e, car(xs)); }
    function pushBegin(fbr, e, xs) { push(fbr, xs); push(fbr, e); push(fbr, KBEGIN); }
    function KBegin() {}; KBegin.prototype.sz = 3; KBegin.prototype.label = "KBEGIN"; var KBEGIN = new KBegin();
    KBegin.prototype.invoke = function(fbr) { var e = idx(fbr, 1); var xs = idx(fbr, 2); ret(fbr); begin1(fbr, e, xs); }
    /***** Objects *****/
    /* Core */
    function Sym(name) { this.name = name; }
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) { return cons.car; }
    function cdr(cons) { return cons.cdr; }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function lookup(e, sym) { var val = e.bindings[sym.name]; return (val !== undefined) ? val : fail("unbound: " + sym.name); }
    function bind(e, lhs, rhs) { lhs.match(e, rhs); }
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
    /* JS Bridge */
    function JSFun(jsfun) { this.jsfun = jsfun; }
    JSFun.prototype.combine = function(fbr, e, o) { acc(fbr, this.jsfun.apply(null, list_to_array(o))); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    /* Data */
    function Str(jsstr) { this.jsstr = jsstr; };
    function str_eql(str1, str2) { return str1.jsstr === str2.jsstr; }
    function str_cat(strings) { return strings.map(function(str) { return str.jsstr; }).join(""); }
    function str_print(str1) { return JSON.stringify(str1.jsstr); }
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
    function init_types(typenames) {
        typenames.map(function (typename) { var type = new Type();
                                            eval(typename).prototype.wat_type = type; }); }
    init_types(["KEval", "KCombine", "KApply", "KEvalArg", "KDef", "KIf", "KBegin",
		"Opv", "Apv", "Def", "Vau", "If", "Eval", "JSFun", "Begin",
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
    function envbind(e, name, val) { bind(e, new Sym(name), val); }
    function mkenvcore() {
	var e = new Env();
	envbind(e, "def", new Def());
	envbind(e, "if", new If());
	envbind(e, "vau", new Vau());
	envbind(e, "eval", wrap(new Eval()));
	envbind(e, "begin", new Begin());
	envbind(e, "wrap", jswrap(wrap));
	envbind(e, "unwrap", jswrap(unwrap));
	envbind(e, "eq?", jswrap(function (a, b) { return (a === b) ? T : F }));
	envbind(e, "cons", jswrap(cons));
	envbind(e, "make-environment", jswrap(function (parent) { return new Env(parent); }));
        envbind(e, "bound?", jswrap(function (sym, e) { return (bound(sym, e)) ? T : F }));
	envbind(e, "make-type", jswrap(make_type));
	envbind(e, "type-of", jswrap(type_of));
	envbind(e, "identity-hash-code", jswrap(function(obj) { return new Num(idhash(obj)); }));
	envbind(e, "display", jswrap(function(str) { console.log(str); return str; }));
	envbind(e, "read-from-string", jswrap(function(str) { return array_to_list(parse(str.jsstr)); }));
	envbind(e, "fail", jswrap(fail));
	envbind(e, "num=", jswrap(function(num1, num2) { return num_eql(num1, num2) ? T : F }));
	envbind(e, "num+", jswrap(num_add));
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
	return e;
    }
    /***** API *****/
    return {
	"eval": function(x, e) { var fbr = new Fbr(e); prime(fbr, e, x); return run(fbr); },
	"mkenvcore": mkenvcore, "parse": parse,
    };
}());