
var wat = (function() {

    // Evaluation

    function evaluate(form, e) {
	var res = perform(form, new KDone(), e); while(typeof(res) === "function") res = res(); return res }

    function perform(form, k, e) { return form.wat_perform ? form.wat_perform(form, k, e) : form }
    Sym.prototype.wat_perform = function(sym, k, e) { return go(k, e, lookup(e, sym)) }
    Xons.prototype.wat_perform = function(xons, k, e) { return perform(car(xons), new KApp(k, xons), e) }

    function KDone() {}
    function KApp(next, opd) { this.next = next; this.opd = opd }
    function KDef(next, name) { this.next = next; this.name = name }
    function KEval1(next, eform) { this.next = next; this.eform = eform }
    function KEval2(next, form) { this.next = next; this.form = form }
    function KJump(k) { this.k = k }
    function go(k, e, val) { return k.wat_go(k, e, val) }
    KDone.prototype.wat_go = function(k, e, val) { return val }
    KApp.prototype.wat_go = function(k, e, opr) { return function() { return operate(opr, k.opd, k.next, e) } }
    KDef.prototype.wat_go = function(k, e, val) { return function() { bind(e, k.name, val); return go(k.next, e, VOID) } }
    KEval1.prototype.wat_go = function(k, e, form) { return function() { return perform(k.eform, new KEval2(k.next, form), e) } }
    KEval2.prototype.wat_go = function(k, e, newe) { return function() { return perform(k.form, k.next, newe) } }
    KJump.prototype.wat_go = function(k, e, val) { return function() { return go(k.k, e, val) } }

    function Fun(param, eparam, body, e) { this.param = param; this.eparam = eparam; this.body = body; this.e = e }
    function Def() {}; function CCC() {}; function Vau() {}; function Eval() {};
    function operate(opr, opd, k, e) { return opr.wat_operate(opr, opd, k, e) }
    Fun.prototype.wat_operate = function(opr, opd, k, e) {
	var xe = extend(opr.e); bind(xe, opr.param, opd); bind(xe, opr.eparam, e); return perform(opr.body, k, xe) }
    Def.prototype.wat_operate = function(opr, opd, k, e) { return perform(elt(opd, 2), new KDef(k, elt(opd, 1))) }
    CCC.prototype.wat_operate = function(opr, opd, k, e) { return perform(elt(opd, 1), new KApp(k, k), e) }
    Vau.prototype.wat_operate = function(opr, opd, k, e) { return go(k, e, new Fun(elt(opd, 1), elt(opd, 2), elt(opd, 3), e)) }
    Eval.prototype.wat_operate = function(opr, opd, k, e) { return perform(elt(opd, 1), new KEval1(k, elt(opd, 2)), e) }

    function koperate(opr, opd, k, e) { return perform(elt(opd, 1), new KJump(opr), e) }
    KDone.prototype.wat_operate = koperate; KApp.prototype.wat_operate = koperate; KDef.prototype.wat_operate = koperate
    KEval1.prototype.wat_operate = koperate; KEval2.prototype.wat_operate = koperate; KK.prototype.wat_operate = koperate

    // Data

    var symtab = Object.create(null)
    function Sym(name) { if (!this instanceof Sym) return intern(name); this.name = name }
    function intern(name) { if (!symtab.name) symtab.name = new Sym(name); return symtab.name }

    function Xons(entries) { if (!this instanceof Xons) return new Xons(entries); this.entries = entries }
    function car(xons) { return xons.car }; function cdr(xons) { return xons.cdr }
    function cons(car, cdr) { return Xons({ car: car, cdr: cdr }) }
    function elt(xons, i) { return (i === 0) car(xons) : elt(cdr(xons), i - 1) }

    function mkenv(parent) { return Object.create(parent ? parent : null) }
    function lookup(e, sym) { return e[sym.name] }
    function bind(e, sym, val) { if (sym !== IGN) e[sym.name] = val }

    var VOID = ["void"]; var IGN = ["ign"]; var NIL = ["nil"]

    function error(err) { throw err }

    // API

    var E = mkenv()
    bind(E, Sym("def"), new Def())
    bind(E, Sym("ccc"), new CCC())
    bind(E, Sym("vau"), new Vau())
    bind(E, Sym("eval"), new Eval())

    return {
	"eval": evaluate, "e": E,
	"car": car, "cdr": cdr, "cons": cons,
	"intern": intern,
	"NIL": NIL, "IGN": IGN, "VOID": VOID,
	"error": error 
    }

}())
