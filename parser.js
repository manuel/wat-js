/* Returns a cons list of the forms in the string. */
function wat_read_from_string(s) {
    return wat_array_to_cons_list(wat_parse(s));
}

/* Returns an array of the forms in the native string. */
function wat_parse(string) {
    var result = wat_program_syntax(ps(string));
    if (result.remaining.index === string.length) {
        return result.ast;
    } else {
        wat.error("Parse error at index: " + result.remaining.index);
    }
}

var wat_expression_syntax =
    function(input) { return wat_expression_syntax(input); }; // forward decl.

var wat_identifier_special_char =
    choice("-", "&", "!", ":", "=", ">", "<", "%",
           "+", "?", "/", "*", "#", "$", "_", "'", ".");

var wat_identifier_char =
    choice(range("a", "z"),
           range("A", "Z"),
           range("0", "9"),
           wat_identifier_special_char);

// Kludge: don't allow single dot as identifier, so as not to conflict
// with dotted pair syntax.
var wat_identifier_syntax =
    action(join_action(butnot(repeat1(wat_identifier_char), "."), ""),
           wat_identifier_syntax_action);

function wat_identifier_syntax_action(ast) {
    return new wat.Sym(ast);
}

var wat_escape_char =
    choice("\"", "\\");

var wat_escape_sequence =
    action(sequence("\\", wat_escape_char),
           wat_escape_sequence_action);

var wat_string_char =
    choice(negate(wat_escape_char),
           wat_escape_sequence);

var wat_string_syntax =
    action(sequence("\"", join_action(repeat0(wat_string_char), ""), "\""),
           wat_string_syntax_action);

function wat_escape_sequence_action(ast) {
    var escape_char = ast[1];
    return escape_char;
}

function wat_string_syntax_action(ast) {
    return new wat.Str(ast[1]);
}

var wat_digits =
    join_action(repeat1(range("0", "9")), "");

var wat_number_syntax =
    action(sequence(optional(choice("+", "-")),
                    wat_digits,
                    optional(join_action(sequence(".", wat_digits), ""))),
           wat_number_syntax_action);

function wat_number_syntax_action(ast) {
    var sign = ast[0] ? ast[0] : "+";
    var integral_digits = ast[1];
    var fractional_digits = ast[2] || "";
    return new wat.Num(Number(sign + integral_digits + fractional_digits));
}

function wat_make_constant_syntax(string, constant) {
    return action(string, function(ast) { return constant; });
}

var wat_void_syntax =
    wat_make_constant_syntax("#void", wat.VOID);

var wat_ign_syntax =
    wat_make_constant_syntax("_", wat.IGN);

var wat_nil_syntax =
    wat_make_constant_syntax("()", wat.NIL);

var wat_dot_syntax =
    action(wsequence(".", wat_expression_syntax),
           wat_dot_syntax_action);

function wat_dot_syntax_action(ast) {
    return ast[1];
}

var wat_compound_syntax =
    action(wsequence("(",
                     repeat1(wat_expression_syntax),
                     optional(wat_dot_syntax),
                     ")"),
           wat_compound_syntax_action);

function wat_is_xons_name(sym) {
    if (sym.wat_tag === wat.Sym.prototype.wat_tag) {
        if (sym.name.indexOf(":") === 0) {
            return true;
        }
    }
    return false;
}

function wat_xons_name_to_name(sym) {
    return sym.name.substr(1);
}

function wat_compound_syntax_action(ast) {
    var exprs = ast[1];
    var end = ast[2] ? ast[2] : wat.NIL;
    /* Process xons name/value pairs. */
    var positional = [];
    var named = {};
    for (var i = 0; i < exprs.length; i++) {
        if (wat_is_xons_name(exprs[i])) {
            //lisp_assert(exprs.length >= i + 1);
            named[wat_xons_name_to_name(exprs[i])] = exprs[i + 1];
            i++;
        } else {
            positional.push(exprs[i]);
        }
    }
    if (positional.length > 0) {
	var result = wat_array_to_cons_list(positional, end);
    } else {
	var result = wat.xons();
    }
    for (var k in named) {
        if (named.hasOwnProperty(k)) {
            result.entries[k] = named[k];
        }
    }
    return result;
}

var wat_line_terminator = choice(ch("\r"), ch("\n"));

var wat_line_comment_syntax =
    action(sequence(";",
                    repeat0(negate(wat_line_terminator)),
                    optional(wat_line_terminator)),
           wat_nothing_action);

var wat_whitespace_syntax =
    action(choice(" ", "\n", "\r", "\t"), wat_nothing_action);

function wat_nothing_action(ast) { // HACK!
    return wat.VOID;
}

var wat_expression_syntax =
    whitespace(choice(wat_number_syntax,
                      wat_nil_syntax,
                      wat_ign_syntax,
                      wat_void_syntax,
                      wat_compound_syntax,
                      wat_identifier_syntax,
                      wat_string_syntax,
                      wat_line_comment_syntax));

var wat_program_syntax =
    whitespace(repeat0(choice(wat_expression_syntax,
                              wat_whitespace_syntax))); // HACK!


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
