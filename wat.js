var wat = (function() {
    ///// Symbols
    function Sym(name) { if (!this instanceof Sym) return intern(name); this.name = name }
    var symtab = Object.create(null)
    function intern(name) { if (!symtab.name) symtab.name = new Sym(name); return symtab.name }
    var VOID = Sym("void"); var IGN = Sym("_")
    ///// Environments
    function mkenv(parent) { return Object.create(parent ? parent : null) }
    function lookup(e, sym) { return e[sym.name] }
    function bind(e, sym, val) { if (sym !== IGN) e[sym.name] = val }
    ///// Evaluation
    function evaluate(form, e) {
	var res = perform(form, new KDone(), e); while(typeof(res) === "function") res = res(); return res }
    function perform(form, k, e) { return form.perform ? form.perform(form, k, e) : form }
    Sym.prototype.perform = function(obj, k, e) { return go(k, e, lookup(e, obj)) }
    Xons.prototype.perform = function(obj, k, e) { return perform(car(obj), new KApp(k, obj), e) }
    ///// Continuations
    function KDone() {}
    function KApp(next, opd) { this.next = next; this.opd = opd }
    function KDef(next, name) { this.next = next; this.name = name }
    function KEval1(next, eform) { this.next = next; this.eform = eform }
    function KEval2(next, form) { this.next = next; this.form = form }
    function KK(k) { this.k = k }
    function go(k, e, val) { return k.go(k, e, val) }
    KDone.prototype.go = function(k, e, val) { return val }
    KApp.prototype.go = function(k, e, opr) { return function() { return operate(opr, k.opd, k.next, e) } }
    KDef.prototype.go = function(k, e, val) { return function() { bind(e, k.name, val); return go(k.next, VOID) } }
    KEval1.prototype.go = function(k, e, form) { return function() { return perform(k.eform, new KEval2(k.next, form), e) } }
    KEval2.prototype.go = function(k, e, newe) { return function() { return perform(k.form, k.next, newe) } }
    KK.prototype.go = function(k, e, val) { return function() { return go(k.k, e, val) } }
    ///// Operaters
    function operate(opr, opd, k, e) { return opr.operate(opr, opd, k, e) }
    function Fun(opd, eopd, body) { this.opd = opd; this.eopd = eopd; this.body = body }
    function Def() {}; function CCC() {}; function Vau() {}; function Eval() {};
    Fun.prototype.operate = function(opr, opd, k, e) {
	var xe = extend(e); bind(xe, opr.opd, opd); bind(xe, opr.eopd, e); return perform(opr.body, k, xe) }
    Def.prototype.operate = function(opr, opd, k, e) { return perform(elt(opd, 2), new KDef(k, elt(opd, 1))) }
    CCC.prototype.operate = function(opr, opd, k, e) { return perform(elt(opd, 1), new KApp(k, k), e) }
    Vau.prototype.operate = function(opr, opd, k, e) { return go(k, new Fun(elt(opd, 1), elt(opd, 2), elt(opd, 3))) }
    Eval.prototype.operate = function(opr, opd, k, e) { return perform(elt(opd, 1), new KEval1(k, elt(opd, 2)), e) }
    function koperate(opr, opd, k, e) { return perform(elt(opd, 1), new KK(opr), e) }
    KDone.prototype.operate = koperate; KApp.prototype.operate = koperate; KDef.prototype.operate = koperate;
    KEval1.prototype.operate = koperate; KEval2.prototype.operate = koperate
    ///// Kernel Environment
    var E = mkenv()
    bind(E, Sym("def"), new Def())
    bind(E, Sym("ccc"), new CCC())
    bind(E, Sym("vau"), new Vau())
    bind(E, Sym("eval"), new Eval())
    ///// API
    return { "eval": evaluate, "std-env": E }
}());
