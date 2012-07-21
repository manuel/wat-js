var wat = (function() {
    function KDone() {}
    function KApp(next, opd) { this.next = next; this.opd = opd }
    function KDef(next, name) { this.next = next; this.name = name }
    function KEval1(next, eexpr) { this.next = next; this.eexpr = eexpr }
    function KEval2(next, expr) { this.next = next; this.expr = expr }
    function enter(k, e, val) { return k.enter(k, e, val) }
    KDone.prototype.enter = function(k, e, val) { return val }
    KApp.prototype.enter = function(k, e, opr) { return function() { return operate(opr, k.opd, k.next, e) } }
    KDef.prototype.enter = function(k, e, val) { return function() { bind(e, k.name, val); return enter(k.next, VOID) } }
    KEval1.prototype.enter = function(k, e, expr) { return function() { return perform(k.eexpr, new KEval2(k.next, expr), e) } }
    KEval2.prototype.enter = function(k, e, newe) { return function() { return perform(k.expr, k.next, newe) } }
    function Lit(val) { if (!this instanceof Lit) return new Lit(val); this.val = val }
    function Sym(name) { if (!this instanceof Sym) return new Sym(name); this.name = name }
    function Xons(entries) { if (!this instanceof Xons) return new Xons(entries); this.entries = entries }
    function perform(expr, k, e) { return expr.perform(expr, k, e) }
    Lit.prototype.perform = function(obj, k, e) { return enter(k, e, obj.val) }
    Sym.prototype.perform = function(obj, k, e) { return enter(k, e, lookup(e, obj)) }
    Xons.prototype.perform = function(obj, k, e) { return perform(car(obj), new KApp(k, obj), e) }
    function operate(opr, opd, k, e) { return opr.operate(opd, k, e) }
    function Fun(formal, eformal, body) { this.formal = formal; this.eformal = eformal; this.body = body }
    function Def() {}; function CCC() {}; function Vau() {}; function Eval() {};
    Fun.prototype.operate = function(opd, k, e) {
	var xe = extend(e); bind(xe, fun.formal, opd); bind(xe, fun.eformal, e); return perform(fun.body, k, xe) }
    Def.prototype.operate = function(opd, k, e) { return perform(elt(opd, 2), new KDef(k, elt(opd, 1))) }
    CCC.prototype.operate = function(opd, k, e) { return perform(elt(opd, 1), new KApp(k, k), e) }
    Vau.prototype.operate = function(opd, k, e) { return enter(k, new Fun(elt(opd, 1), elt(opd, 2), elt(opd, 3))) }
    Eval.prototype.operate = function(opd, k, e) { return perform(elt(opd, 1), new KEval1(k, elt(opd, 2)), e) }
    function koperate(opd, k, e) { return enter(k, e, elt(opd, 1)) }
    KDone.prototype.operate = koperate; KApp.prototype.operate = koperate; KDef.prototype.operate = koperate;
    KEval1.prototype.operate = koperate; KEval2.prototype.operate = koperate
    function mkenv(parent) { return Object.create(parent ? parent : null) }
    function lookup(e, sym) { return e[sym.name] }
    function bind(e, sym, value) { if (sym.name !== IGN) e[sym.name] = value }
    var VOID = Sym("void"); var IGN = Sym("_")
    function evaluate(expr, e) {
	var res = perform(expr, new KDone(), e); while(typeof(res) === "function") res = res(); return res }
    var E = mkenv()
    bind(E, Sym("def"), new Def())
    bind(E, Sym("ccc"), new CCC())
    bind(E, Sym("vau"), new Vau())
    bind(E, Sym("eval"), new Eval())
    return { "eval": evaluate, "e": E }
}());
