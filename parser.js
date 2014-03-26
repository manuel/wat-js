var jsparse = require("./jsparse.js");
module.exports.parse_sexp = parse_sexp;

var ps = jsparse.ps; var choice = jsparse.choice; var range = jsparse.range; var action = jsparse.action; var sequence = jsparse.sequence; var join = jsparse.join; var join_action = jsparse.join_action; var negate = jsparse.negate; var repeat0 = jsparse.repeat0; var optional = jsparse.optional; var repeat1 = jsparse.repeat1; var wsequence = jsparse.wsequence; var whitespace = jsparse.whitespace; var ch = jsparse.ch; var butnot = jsparse.butnot;

/* S-expr parser */
function parse_sexp(s) {
    var res = program_stx(ps(s));
    if (res.remaining.index === s.length) return res.ast;
    else throw("parse error at " + res.remaining.index + " in " + s); }
var x_stx = function(input) { return x_stx(input); }; // forward decl.
var id_special_char =
    choice("-", "&", "!", "=", ">", "<", "%", "+", "?", "/", "*", "$", "_", "'", ".", "@", "|", "~", "^");
var id_char = choice(range("a", "z"), range("A", "Z"), range("0", "9"), id_special_char);
// Kludge: don't allow single dot as id, so as not to conflict with dotted pair stx.
var id_stx = action(join_action(butnot(repeat1(id_char), "."), ""), handle_identifier);
function handle_identifier(str) {
    if ((str[0] === ".") && (str.length > 1)) { return ["js-getter", ["wat-string", str.substring(1)]]; }
    else if (str[0] === "~") { return ["js-invoker", ["wat-string", str.substring(1)]]; }
    else if (str[0] === "$") { return ["js-global", ["wat-string", str.substring(1)]]; }
    else return str; }
var escape_char = choice("\"", "\\", "n", "r", "t");
var escape_sequence = action(sequence("\\", escape_char), function (ast) {
    switch(ast[1]) {
    case "n": return "\n";
    case "r": return "\r";
    case "t": return "\t";
    default: return ast[1]; }});
var line_terminator = choice(ch("\r"), ch("\n"));
var string_char = choice(escape_sequence, line_terminator, negate("\""));
var string_stx = action(sequence("\"", join_action(repeat0(string_char), ""), "\""),
                        function (ast) { return ["wat-string", ast[1]]; });
var digits = join_action(repeat1(range("0", "9")), "");
var number_stx =
    action(sequence(optional(choice("+", "-")), digits, optional(join_action(sequence(".", digits), ""))),
           function (ast) {
               var sign = ast[0] ? ast[0] : "";
               var integral_digits = ast[1]; 
               var fractional_digits = ast[2] || "";
               return Number(sign + integral_digits + fractional_digits); });
function make_constant_stx(string, constant) { return action(string, function(ast) { return constant; }); }
var nil_stx = make_constant_stx("()", []);
var ign_stx = make_constant_stx("#ignore", "#ignore");
var t_stx = make_constant_stx("#t", true);
var f_stx = make_constant_stx("#f", false);
var null_stx = make_constant_stx("#null", null);
var undef_stx = make_constant_stx("#undefined", undefined);
var dot_stx = action(wsequence(".", x_stx), function (ast) { return ast[1]; });
var qualified_stx = action(sequence(id_stx, ":", id_stx), function(ast) {
    return ["eval", ["quote", ast[2]], ast[0]]; });
var compound_stx = action(wsequence("(", repeat1(x_stx), optional(dot_stx), ")"),
                          function(ast) {
                              var exprs = ast[1];
                              var end = ast[2] ? [".", ast[2]] : [];
                              return exprs.concat(end); });
var quote_stx = action(sequence("'", x_stx), function(ast) { return ["quote", ast[1]]; });
var cmt_stx = action(sequence(";", repeat0(negate(line_terminator)), optional(line_terminator)), nothing_action);
var whitespace_stx = action(choice(" ", "\n", "\r", "\t"), nothing_action);
function nothing_action(ast) { return null; } // HACK!
var x_stx = whitespace(choice(qualified_stx, ign_stx, nil_stx, t_stx, f_stx, null_stx, undef_stx, number_stx,
                              quote_stx, compound_stx, id_stx, string_stx, cmt_stx));
var program_stx = whitespace(repeat0(choice(x_stx, whitespace_stx))); // HACK!
