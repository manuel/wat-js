var wat = (function() {
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
    function jsapv(jsfun) { return new Apv(new JSFun(jsfun)); }

    function Sym(name) { this.name = name; }
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) { return cons.car; };
    function cdr(cons) { return cons.cdr; }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); }
    function mkenv(parent) { return new Env(parent); }
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
    function Tag() { this.e = new Env() };
    function mktag() { return new Tag(); };
    function tag_env(tag) { return tag.e; };
    function Tagged(tag, val) { this.wat_tag = tag; this.val = val };
    function tag(tag, val) { return new Tagged(tag, val); };
    function tag_of(obj) { return obj.wat_tag; }
    function fail(err) { throw err; }
    function array_to_list(array, end) {
	var c = end ? end : wat.NIL; for (var i = array.length; i > 0; i--) c = wat.cons(array[i - 1], c); return c; }
    function list_to_array(c) {
	var res = []; while(c !== wat.NIL) { res.push(wat.car(c)); c = wat.cdr(c); } return res; }
    function reverse_list(list) {
	var res = wat.NIL; while(list !== wat.NIL) { res = wat.cons(wat.car(list), res); list = wat.cdr(list); } return res; }
    
    function mkenvcore() {
	var e = new Env();
	bind(e, new Sym("def"), new Def());
	bind(e, new Sym("if"), new If());
	bind(e, new Sym("ccc"), new Apv(new CCC()));
	bind(e, new Sym("jump"), new Apv(new Jump()));
	bind(e, new Sym("vau"), new Vau());
	bind(e, new Sym("eval"), new Apv(new Eval()));
	bind(e, new Sym("eq"), jsapv(function (a, b) { return (a === b) ? T : F }));
	bind(e, new Sym("cons"), jsapv(cons));
	bind(e, new Sym("car"), jsapv(car));
	bind(e, new Sym("cdr"), jsapv(cdr));
	bind(e, new Sym("wrap"), jsapv(wrap));
	bind(e, new Sym("unwrap"), jsapv(unwrap));
	bind(e, new Sym("mkenv"), jsapv(mkenv));
	bind(e, new Sym("mktag"), jsapv(mktag));
	bind(e, new Sym("tag-env"), jsapv(tag_env));
	bind(e, new Sym("tag-of"), jsapv(tag_of));
	bind(e, new Sym("tag"), jsapv(tag));
	bind(e, new Sym("fail"), jsapv(fail));
	return e;
    }

    // API

    return {
	"eval": function(x, e) { var fbr = new Fbr(); fbr.prime(x, e); return fbr.run(); },
	"mkenvcore": mkenvcore,
	"car": car, "cdr": cdr, "cons": cons,
	"Sym": Sym, "Cons": Cons, "Str": Str, "Num": Num,
	"VOID": VOID, "IGN": IGN, "NIL": NIL, "T": T, "F": F,
	"array_to_list": array_to_list,
    };
}());
