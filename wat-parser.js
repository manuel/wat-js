var jsparse = require("./jsparse.js");
module.exports.parse_sexp = parse_sexp;

var ps = jsparse.ps; var choice = jsparse.choice; var range = jsparse.range; var action = jsparse.action; var sequence = jsparse.sequence; var join = jsparse.join; var join_action = jsparse.join_action; var negate = jsparse.negate; var repeat0 = jsparse.repeat0; var optional = jsparse.optional; var repeat1 = jsparse.repeat1; var wsequence = jsparse.wsequence; var whitespace = jsparse.whitespace; var ch = jsparse.ch;

/* S-expr parser */
function parse_sexp(s) {
    var res = program_stx(ps(s));
    if (res.remaining.index === s.length) return ["begin"].concat(res.ast);
    else throw("parse error at " + res.remaining.index + " in " + s); }
var x_stx = function(input) { return x_stx(input); }; // forward decl.
var id_special_char =
    choice("-", "&", "!", ":", "=", ">", "<", "%", "+", "?", "/", "*", "#", "$", "_", "'", ".", "@", "|", "~", "^");
var id_char = choice(range("a", "z"), range("A", "Z"), range("0", "9"), id_special_char);
var id_stx = action(join_action(repeat1(id_char), ""), function (ast) { return ast; });
var escape_char = choice("\"", "\\", "n", "r", "t");
var escape_sequence = action(sequence("\\", escape_char), function (ast) {
    switch(ast[1]) {
    case "n": return "\n";
    case "r": return "\r";
    case "t": return "\t";
    default: return ast[1]; }
});
var line_terminator = choice(ch("\r"), ch("\n"));
var string_char = choice(escape_sequence, negate("\""), line_terminator);
var string_stx =
    action(sequence("\"", join_action(repeat0(string_char), ""), "\""),
           function (ast) { return ["string", ast[1]]; });
var digits = join_action(repeat1(range("0", "9")), "");
var number_stx =
    action(sequence(optional(choice("+", "-")), digits, optional(join_action(sequence(".", digits), ""))),
           function (ast) {
               var sign = ast[0] ? ast[0] : "";
               var integral_digits = ast[1]; 
               var fractional_digits = ast[2] || "";
               return Number(sign + integral_digits + fractional_digits); });
function make_constant_stx(string, constant) { return action(string, function(ast) { return constant; }); }
var ign_stx = make_constant_stx("ignore", "ignore");
var nil_stx = make_constant_stx("()", []);
var t_stx = make_constant_stx("true", true);
var f_stx = make_constant_stx("false", false);
var null_stx = make_constant_stx("null", null);
var undef_stx = make_constant_stx("undefined", undefined);
var compound_stx = action(wsequence("(", repeat1(x_stx), ")"), function(ast) { return ast[1]; });
var quote_stx = action(sequence("'", x_stx), function(ast) { return ["quote", ast[1]]; });
var cmt_stx = action(sequence(";", repeat0(negate(line_terminator)), optional(line_terminator)), nothing_action);
var whitespace_stx = action(choice(" ", "\n", "\r", "\t"), nothing_action);
function nothing_action(ast) { return null; } // HACK!
var x_stx = whitespace(choice(ign_stx, nil_stx, t_stx, f_stx, null_stx, undef_stx, number_stx,
                              quote_stx, compound_stx, id_stx, string_stx, cmt_stx));
var program_stx = whitespace(repeat0(choice(x_stx, whitespace_stx))); // HACK!
