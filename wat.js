// -*- fill-column: 100 -*-

var wat = (function() {

    function run(form, e) {
	var res = ev(form, new KDone(), e); while(typeof(res) === "function") res = res(); return res }
    function ev(form, k, e) { return form.ev ? form.ev(form, k, e) : form }
    Sym.prototype.ev = function(form, k, e) { return go(k, e, lookup(e, form)) }
    Cons.prototype.ev = function(form, k, e) { return ev(car(form), new KOp(k, cdr(form)), e) }
    function Cmb(p, ep, body, e) { this.p = p; this.ep = ep; this.body = body; this.e = e }
    function Apl(cmb) { this.cmb = cmb };
    function Def() {}; function CCC() {}; function Vau() {}; function Eval() {}
    function op(cmb, opd, k, e) { return cmb.op(cmb, opd, k, e) }
    Cmb.prototype.op = function(cmb, opd, k, e) {
	var xe = new Env(cmb.e); bind(xe, cmb.p, opd); bind(xe, cmb.ep, e); return ev(cmb.body, k, xe) }
    Apl.prototype.op = function(cmb, opd, k, e) {
	if (opd === NIL) return op(cmb.cmb, NIL, k, e) else return ev(car(opd), new KArg(k, cmb.cmb, cdr(opd), NIL), e) }
    Def.prototype.op = function(cmb, opd, k, e) { return ev(elt(opd, 1), new KDef(k, elt(opd, 0)), e) }
    CCC.prototype.op = function(cmb, opd, k, e) { return op(elt(opd, 0), new Apl(k), k, e) }
    Vau.prototype.op = function(cmb, opd, k, e) { return go(k, e, new Cmb(elt(opd, 0), elt(opd, 1), elt(opd, 2), e)) }
    Eval.prototype.op = function(cmb, opd, k, e) { return ev(elt(opd, 0), k, elt(opd, 1)) }
    function KDone() {}
    function KOp(k, opd) { this.k = k; this.opd = opd }
    function KArg(k, cmb, todo, done) { this.k = k; this.cmb = cmb; this.todo = todo; this.done = done }
    function KDef(k, name) { this.k = k; this.name = name }
    function go(k, e, val) { return k.go(k, e, val) }
    KDone.prototype.go = function(k, e, val) { return val }
    KOp.prototype.go = function(k, e, cmb) { return function() { return op(cmb, k.opd, k.k, e) } }
    KArg.prototype.go = function(k, e, arg) { return function() {
	if (k.todo === NIL) return op(k.cmb, wat_reverse_cons_list(cons(arg, k.done)), k.k, e)
	else return ev(car(k.todo), new KArg(k.k, k.cmb, cdr(k.todo), cons(arg, k.done))) } }
    KDef.prototype.go = function(k, e, val) { return function() { bind(e, k.name, val); return go(k.k, e, VOID) } }
    function kop(cmb, opd, k, e) { return go(cmb, elt(opd, 0), e) }
    KDone.prototype.op = kop; KOp.prototype.op = kop; KArg.prototype.op = kop; KDef.prototype.op = kop

    function JSFun(jsfun) { this.jsfun = jsfun }
    JSFun.prototype.op = function(cmb, opd, k, e) {
	return go(k, e, cmb.jsfun.apply(null, wat_cons_list_to_array(opd).reverse())) }

    /***** Data *****/

    function Sym(name) { this.name = name }

    function Cons(car, cdr) { this.car = car; this.cdr = cdr }
    function car(cons) { return cons.car }; function cdr(cons) { return cons.cdr }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1) }

    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null) }
    function lookup(e, sym) { return e.bindings[sym.name] }
    function bind(e, sym, val) { if (sym !== IGN) e.bindings[sym.name] = val }

    function Str(jsstr) { this.jsstr = jsstr }
    function Num(jsnum) { this.jsnum = jsnum }

    function Void() {}; function Ign() {}; function Nil() {}; function True() {}; function False() {}
    var VOID = new Void(); var IGN = new Ign(); var NIL = new Nil(); var T = new True(); var F = new False()

    function Tag() { this.e = new Env() }
    function Tagged(tag, val) { this.wat_tag = tag; this.val = val }
    
    Tag.prototype.wat_tag = new Tag()
    Sym.prototype.wat_tag = new Tag()
    Cons.prototype.wat_tag = new Tag()
    Env.prototype.wat_tag = new Tag()
    Str.prototype.wat_tag = new Tag()
    Num.prototype.wat_tag = new Tag()
    Void.prototype.wat_tag = new Tag()
    Ign.prototype.wat_tag = new Tag()
    Nil.prototype.wat_tag = new Tag()
    True.prototype.wat_tag = new Tag()
    False.prototype.wat_tag = new Tag()
    Opr.prototype.wat_tag = new Tag()
    Def.prototype.wat_tag = new Tag()
    CCC.prototype.wat_tag = new Tag()
    Vau.prototype.wat_tag = new Tag()
    Eval.prototype.wat_tag = new Tag()
    KDone.prototype.wat_tag = new Tag()
    KApp.prototype.wat_tag = new Tag()
    KDef.prototype.wat_tag = new Tag()
    KEval1.prototype.wat_tag = new Tag()
    KEval2.prototype.wat_tag = new Tag()
    KJump.prototype.wat_tag = new Tag()
    JSFun.prototype.wat_tag = new Tag()
    KJSApp.prototype.wat_tag = new Tag()

    // Core Environment

    var lib_eq = new JSFun(function (e, a, b) { return (a === b) ? T : F })
    var lib_cons = new JSFun(function (e, a, b) { return cons(a, b) })
    var lib_car = new JSFun(function (e, cons) { return car(cons) })
    var lib_cdr = new JSFun(function (e, cons) { return cdr(cons) })
    var lib_mkenv = new JSFun(function (e, parent) { return new Env(parent !== VOID ? parent : null) })
    var lib_mktag = new JSFun(function (e) { return new Tag() })
    var lib_tagenv = new JSFun(function (e, tag) { return tag.e })
    var lib_tag_of = new JSFun(function (e, obj) { return obj.wat_tag })
    var lib_tag = new JSFun(function (e, tag, val) { return new Tagged(tag, val) })
    var lib_fail = new JSFun(function (e, err) { throw err })

    function mkenvcore() {
	var e = new Env()
	bind(e, new Sym("def"), new Def())
	bind(e, new Sym("ccc"), new Apl(new CCC()))
	bind(e, new Sym("vau"), new Vau())
	bind(e, new Sym("eval"), new Apl(new Eval()))
	bind(e, new Sym("eq"), lib_eq)
	bind(e, new Sym("cons"), lib_cons)
	bind(e, new Sym("car"), lib_car)
	bind(e, new Sym("cdr"), lib_cdr)
	bind(e, new Sym("mkenv"), lib_mkenv)
	bind(e, new Sym("mktag"), lib_mktag)
	bind(e, new Sym("tagenv"), lib_tagenv)
	bind(e, new Sym("tag-of"), lib_tag_of)
	bind(e, new Sym("tag"), lib_tag)
	bind(e, new Sym("fail"), lib_fail)
	return e
    }

    // API

    return {
	"eval": evaluate, "mkenvcore": mkenvcore,
	"car": car, "cdr": cdr, "cons": cons,
	"Sym": Sym, "Cons": cons, "Str": Str, "Num": Num,
	"VOID": VOID, "IGN": IGN, "NIL": NIL, "T": T, "F": F
    }

}())

function wat_array_to_cons_list(array, end) {
    var c = end ? end : wat.NIL;
    for (var i = array.length; i > 0; i--)
        c = wat.cons(array[i - 1], c);
    return c;
}

function wat_cons_list_to_array(c) {
    var res = [];
    while(c !== wat.NIL) {
        res.push(wat.car(c));
        c = wat.cdr(c);
    }
    return res;
}

function wat_reverse(list) {
    var res = wat.NIL;
    while(list !== NIL) {
	res = wat.cons(wat.car(list), res);
	list = wat.cdr(list);
    }
    return res;
}
