!function(e){if("object"==typeof exports)module.exports=e();else if("function"==typeof define&&define.amd)define(e);else{var f;"undefined"!=typeof window?f=window:"undefined"!=typeof global?f=global:"undefined"!=typeof self&&(f=self),f.wat=e()}}(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(_dereq_,module,exports){
module.exports.main = [null,null,null,["vm-def","_define","vm-def"],null,["_define","array-to-list","vm-array-to-list"],["_define","begin","vm-begin"],["_define","cons","vm-cons"],["_define","cons?","vm-cons?"],["_define","dnew","vm-dnew"],["_define","dref","vm-dref"],["_define","error","vm-error"],["_define","eval","vm-eval"],["_define","if","vm-if"],["_define","js-getter","vm-js-getter"],["_define","js-global","vm-js-global"],["_define","js-invoker","vm-js-invoker"],["_define","list*","vm-list*"],["_define","list-to-array","vm-list-to-array"],["_define","make-environment","vm-make-environment"],["_define","new","vm-js-new"],["_define","nil?","vm-nil?"],["_define","reverse-list","vm-reverse-list"],["_define","setter","vm-setter"],["_define","symbol-name","vm-symbol-name"],["_define","symbol?","vm-symbol?"],["_define","throw","vm-throw"],["_define","unwrap","vm-unwrap"],["_define","wrap","vm-wrap"],null,["_define","quote",["vm-vau",["x"],"#ignore","x"]],["_define","list",["wrap",["vm-vau","elts","#ignore","elts"]]],["_define","the-environment",["vm-vau",[],"e","e"]],null,["_define","make-macro-expander",["wrap",["vm-vau",["expander"],"#ignore",["vm-vau","operands","env",["eval",["eval",["cons","expander","operands"],["make-environment"]],"env"]]]]],["_define","_vau",["make-macro-expander",["vm-vau",["params","env-param",".","body"],"#ignore",["list","vm-vau","params","env-param",["list*","begin","body"]]]]],["_define","macro",["make-macro-expander",["_vau",["params",".","body"],"#ignore",["list","make-macro-expander",["list*","_vau","params","#ignore","body"]]]]],["_define","define-macro",["macro",[["name",".","params"],".","body"],["list","_define","name",["list*","macro","params","body"]]]],["define-macro",["_lambda","params",".","body"],["list","wrap",["list*","_vau","params","#ignore","body"]]],["define-macro",["define-operative",["name",".","params"],"envparam",".","body"],["list","_define","name",["list*","_vau","params","envparam","body"]]],null,["define-macro",["loop",".","body"],["list","vm-loop",["list*","begin","body"]]],["define-operative",["catch","protected","handler"],"env",["eval",["list","vm-catch","protected",["eval","handler","env"]],"env"]],["define-operative",["push-prompt","prompt",".","body"],"env",["eval",["list","vm-push-prompt",["eval","prompt","env"],["list*","begin","body"]],"env"]],["define-macro",["take-subcont","prompt","k",".","body"],["list","vm-take-subcont","prompt",["list*","_lambda",["list","k"],"body"]]],["define-macro",["push-subcont","k",".","body"],["list","vm-push-subcont","k",["list*","_lambda",[],"body"]]],["define-macro",["push-prompt-subcont","p","k",".","body"],["list","vm-push-prompt-subcont","p","k",["list*","_lambda",[],"body"]]],null,["_define","compose",["_lambda",["f","g"],["_lambda",["arg"],["f",["g","arg"]]]]],["_define","car",["_lambda",[["x",".","#ignore"]],"x"]],["_define","cdr",["_lambda",[["#ignore",".","x"]],"x"]],["_define","caar",["compose","car","car"]],["_define","cadr",["compose","car","cdr"]],["_define","cdar",["compose","cdr","car"]],["_define","cddr",["compose","cdr","cdr"]],null,["_define","map-list",["_lambda",["f","lst"],["if",["nil?","lst"],[],["cons",["f",["car","lst"]],["map-list","f",["cdr","lst"]]]]]],["_define","list-keep",["_lambda",["p","lst"],["if",["nil?","lst"],[],["if",["p",["car","lst"]],["cons",["car","lst"],["list-keep","p",["cdr","lst"]]],["list-keep","p",["cdr","lst"]]]]]],["_define","fold-list",["_lambda",["f","init","lst"],["if",["nil?","lst"],"init",["fold-list","f",["f","init",["car","lst"]],["cdr","lst"]]]]],["define-macro",["let","x",".","rest"],["if",["symbol?","x"],["list*","let-loop","x","rest"],["list*",["list*","_lambda",["map-list","car","x"],"rest"],["map-list","cadr","x"]]]],["define-macro",["let-loop","name","bindings",".","body"],["list","letrec",["list",["list","name",["list*","lambda",["map-list","car","bindings"],"body"]]],["list*","name",["map-list","cadr","bindings"]]]],["define-macro",["let*","bindings",".","body"],["if",["nil?","bindings"],["list*","let",[],"body"],["list","let",["list",["car","bindings"]],["list*","let*",["cdr","bindings"],"body"]]]],["define-macro",["letrec","bindings",".","body"],["list*","let",[],["list","_define",["map-list","car","bindings"],["list*","list",["map-list","cadr","bindings"]]],"body"]],["define-macro",["lambda","params",".","body"],["letrec",[["typed-params->names-and-checks",["_lambda",["ps"],["if",["cons?","ps"],["let*",[[["p",".","rest-ps"],"ps"],[["names",".","checks"],["typed-params->names-and-checks","rest-ps"]]],["if",["cons?","p"],["let*",[[["name","type"],"p"],["check",["list","the","type","name"]]],["cons",["cons","name","names"],["cons","check","checks"]]],["cons",["cons","p","names"],"checks"]]],["cons","ps",[]]]]]],["let",[[["untyped-names",".","type-checks"],["typed-params->names-and-checks","params"]]],["list*","_lambda","untyped-names",["list*","begin","type-checks"],"body"]]]],["define-macro",["define","lhs",".","rhs"],["if",["cons?","lhs"],["list","_define",["car","lhs"],["list*","lambda",["cdr","lhs"],"rhs"]],["list","_define","lhs",["car","rhs"]]]],["define",["apply","appv","arg",".","opt"],["if",["instanceof","appv",["js-global",["wat-string","Function"]]],[["js-invoker",["wat-string","apply"]],"appv",null,["list-to-array","arg"]],["eval",["cons",["unwrap","appv"],"arg"],["if",["nil?","opt"],["make-environment"],["car","opt"]]]]],null,["define-operative",["cond",".","clauses"],"env",["if",["nil?","clauses"],null,["let",[[[["test",".","body"],".","clauses"],"clauses"]],["if",["eval","test","env"],["apply",["wrap","begin"],"body","env"],["apply",["wrap","cond"],"clauses","env"]]]]],["define","else",true],["define-operative",["and",".","x"],"e",["cond",[["nil?","x"],true],[["nil?",["cdr","x"]],["eval",["car","x"],"e"]],[["eval",["car","x"],"e"],["apply",["wrap","and"],["cdr","x"],"e"]],["else",false]]],["define-operative",["or",".","x"],"e",["cond",[["nil?","x"],false],[["nil?",["cdr","x"]],["eval",["car","x"],"e"]],[["eval",["car","x"],"e"],true],["else",["apply",["wrap","or"],["cdr","x"],"e"]]]],["define",["call-with-escape","fun"],["let",[["fresh",["list",null]]],["catch",["fun",["_lambda","opt-arg",["throw",["list","fresh","opt-arg"]]]],["_lambda",["exc"],["if",["and",["cons?","exc"],["=","fresh",["car","exc"]]],["let",[["opt-arg",["cadr","exc"]]],["if",["cons?","opt-arg"],["car","opt-arg"],null]],["throw","exc"]]]]]],["define-macro",["label","name",".","body"],["list","call-with-escape",["list*","_lambda",["list","name"],"body"]]],["define-operative",["while","test",".","body"],"env",["let",[["body",["list*","begin","body"]]],["label","return",["loop",["if",["eval","test","env"],["eval","body","env"],["return"]]]]]],["define-macro",["when","test",".","body"],["list","if","test",["list*","begin","body"],null]],["define-macro",["unless","test",".","body"],["list*","when",["list","not","test"],"body"]],["define-macro",["set",["getter",".","args"],"new-val"],["list*",["list","setter","getter"],"new-val","args"]],null,null,["define-operative",["dlet","bindings",".","body"],"env",["eval",["let","process-bindings",[["bs","bindings"]],["if",["nil?","bs"],["list*","begin","body"],["let*",[[[["name","expr"],".","rest-bs"],"bs"],["value",["eval","expr","env"]]],["list","vm-dlet","name","value",["process-bindings","rest-bs"]]]]],"env"]],null,["define-operative",["define-prototype","name","super-name","prop-names"],"env",["let",[["p",["apply","vm-js-make-prototype",["list*",["symbol-name","name"],["map-list","symbol-name","prop-names"]]]],["super",["eval","super-name","env"]]],["set",[["js-getter",["wat-string","prototype"]],"p"],[["js-invoker",["wat-string","create"]],["js-global",["wat-string","Object"]],[["js-getter",["wat-string","prototype"]],"super"]]],["set",[["js-getter",["wat-string","constructor"]],[["js-getter",["wat-string","prototype"]],"p"]],"super"],["eval",["list","_define","name","p"],"env"]]],["define",["put-method","ctor","name","js-fun"],["set",[["js-getter","name"],[["js-getter",["wat-string","prototype"]],"ctor"]],"js-fun"]],["define-macro",["define-method",["name",["self","ctor"],".","args"],".","body"],["list","put-method","ctor",["symbol-name","name"],["list","vm-js-function",["list*","lambda",["list*","self","args"],"body"]]]],["define-macro",["define-generic",["name",".","#ignore"]],["list","_define","name",["vm-js-invoker",["symbol-name","name"]]]],null,["define-operative",["provide","symbols",".","body"],"env",["eval",["list","_define","symbols",["list","let",[],["list*","begin","body"],["list*","list","symbols"]]],"env"]],["define-operative",["module","exports",".","body"],"env",["let",[["menv",["make-environment","env"]]],["eval",["list*","provide","exports","body"],"menv"],["make-environment","menv"]]],["define-macro",["define-module","name","exports",".","body"],["list","_define","name",["list*","module","exports","body"]]],["define-operative",["import","module","imports"],"env",["let*",[["m",["eval","module","env"]],["values",["map-list",["_lambda",["import"],["eval","import","m"]],"imports"]]],["eval",["list","_define","imports",["list*","list","values"]],"env"]]],null,["define",["relational-op","name"],["let",[["binop",["vm-js-binop","name"]]],["letrec",[["op",["lambda",["arg1","arg2",".","rest"],["if",["binop","arg1","arg2"],["if",["nil?","rest"],true,["apply","op",["list*","arg2","rest"]]],false]]]],"op"]]],["define","=",["relational-op",["wat-string","==="]]],["define","<",["relational-op",["wat-string","<"]]],["define",">",["relational-op",["wat-string",">"]]],["define","<=",["relational-op",["wat-string","<="]]],["define",">=",["relational-op",["wat-string",">="]]],["define",["!=",".","args"],["not",["apply","=","args"]]],["define","*",["let",[["vm*",["vm-js-binop",["wat-string","*"]]]],["lambda","args",["fold-list","vm*",1,"args"]]]],null,["define","+",["let",[["vm+",["vm-js-binop",["wat-string","+"]]]],["lambda","args",["if",["nil?","args"],0,["fold-list","vm+",["car","args"],["cdr","args"]]]]]],["define",["negative-op","binop","unit"],["lambda",["arg1",".","rest"],["if",["nil?","rest"],["binop","unit","arg1"],["fold-list","binop","arg1","rest"]]]],["define","-",["negative-op",["vm-js-binop",["wat-string","-"]],0]],["define","/",["negative-op",["vm-js-binop",["wat-string","/"]],1]],["define","%",["vm-js-binop",["wat-string","%"]]],["define","not",["vm-js-unop",["wat-string","!"]]],["define","typeof",["vm-js-unop",["wat-string","typeof"]]],["define","in",["vm-js-binop",["wat-string","in"]]],["define","instanceof",["vm-js-binop",["wat-string","instanceof"]]],["define","bitand",["vm-js-binop",["wat-string","&"]]],["define","bitor",["vm-js-binop",["wat-string","|"]]],["define","bitxor",["vm-js-binop",["wat-string","^"]]],["define","bitnot",["vm-js-unop",["wat-string","~"]]],["define","bitshiftl",["vm-js-binop",["wat-string","<<"]]],["define","bitshiftr",["vm-js-binop",["wat-string",">>"]]],["define","bitshiftr0",["vm-js-binop",["wat-string",">>>"]]],["define-operative",["object",".","pairs"],"env",["let",[["obj",["vm-js-make-object"]]],["map-list",["_lambda",[["name","value"]],["set",[["js-getter",["eval","name","env"]],"obj"],["eval","value","env"]]],"pairs"],"obj"]],["define",["elt","object","key"],[["js-getter","key"],"object"]],["set",["setter","elt"],["lambda",["new-val","object","key"],["set",[["js-getter","key"],"object"],"new-val"]]],["define",["array",".","args"],["list-to-array","args"]],["define",["js-callback","fun"],["vm-js-function",["_lambda","args",["push-prompt","vm-root-prompt",["apply","fun","args"]]]]],["define-macro",["type?","obj","type"],["list","vm-type?","obj","type",["symbol-name","type"]]],["define-macro",["the","type","obj"],["list","if",["list","type?","obj","type"],"obj",["list","error",["list","+","obj",["wat-string"," is not a: "],"type"]]]],["define","Array",["js-global",["wat-string","Array"]]],["define","Boolean",["js-global",["wat-string","Boolean"]]],["define","Date",["js-global",["wat-string","Date"]]],["define","Function",["js-global",["wat-string","Function"]]],["define","Number",["js-global",["wat-string","Number"]]],["define","Object",["js-global",["wat-string","Object"]]],["define","RegExp",["js-global",["wat-string","RegExp"]]],["define","String",["js-global",["wat-string","String"]]],["define",["log","x",".","xs"],["apply",["js-invoker",["wat-string","log"]],["list*",["js-global",["wat-string","console"]],"x","xs"]],"x"],null,["define-prototype","Cell","Object",["value"]],["define",["cell","value"],["new","Cell","value"]],["define",["ref",["c","Cell"]],[["js-getter",["wat-string","value"]],"c"]],["set",["setter","ref"],["lambda",["new-val",["c","Cell"]],["set",[["js-getter",["wat-string","value"]],"c"],"new-val"]]],["define-macro",["++","place"],["list","set","place",["list","+","place",1]]],["define-macro",["--","place"],["list","set","place",["list","-","place",1]]],null,null,["define",["map-array","fun",["arr","Array"]],["list-to-array",["map-list","fun",["array-to-list","arr"]]]],["define",["array-keep","pred",["arr","Array"]],["list-to-array",["list-keep","pred",["array-to-list","arr"]]]],["define-operative",["time","expr"],"env",["let",[["n",[["js-invoker",["wat-string","getTime"]],["new","Date"]]],["result",["eval","expr","env"]]],["log",["+",["wat-string","time "],"expr",["wat-string",": "],["-",[["js-invoker",["wat-string","getTime"]],["new","Date"]],"n"],["wat-string","ms"]]],"result"]],null,["define",["print-stacktrace"],["define",["print-frame","k"],["log",[["js-invoker",["wat-string","toString"]],[["js-getter",["wat-string","dbg"]],"k"]],[["js-getter",["wat-string","e"]],"k"]],["if",[["js-getter",["wat-string","next"]],"k"],["print-frame",[["js-getter",["wat-string","next"]],"k"]],null]],["take-subcont","vm-root-prompt","k",["print-frame","k"],["push-prompt","vm-root-prompt",["push-subcont","k"]]]],["define",["user-break","err"],["print-stacktrace"],["throw","err"]],null,["define-operative",["let-redirect","exp","bindings",".","body"],"env",["eval",["list*",["eval",["list*","_lambda",["map-list","car","bindings"],"body"],["eval","exp","env"]],["map-list","cadr","bindings"]],"env"]],["define-operative",["bindings->environment",".","bindings"],"denv",["eval",["list","let-redirect",["make-environment"],"bindings",["list","the-environment"]],"denv"]],["define-operative",["slurp-environment",".","bindings"],"env",["eval",["list*","bindings->environment",["map-list",["lambda",["b"],["list","b","b"]],"bindings"]],"env"]],null,null,null,["make-environment",["slurp-environment","define-operative","_define","_lambda","_vau","apply","eval","make-environment","the-environment","unwrap","wrap","begin","define","define-macro","lambda","let","let*","letrec","quote","symbol-name","symbol?","caar","cadr","car","cdar","cddr","cdr","cons","cons?","fold-list","list","list*","map-list","list-keep","nil?","reverse-list","define-generic","define-prototype","define-method","new","the","type?","catch","cond","else","if","label","loop","throw","unless","when","while","error","set","setter","push-prompt","push-subcont","take-subcont","push-prompt-subcont","dlet","dnew","dref","define-module","import","module","provide","Array","Date","Function","Number","Object","RegExp","String","array","array-to-list","map-array","array-keep","js-callback","js-getter","js-global","js-invoker","list-to-array","object","log","elt","and","or","not","!=","%","*","+","-","/","<","<=","=",">",">=","in","instanceof","typeof","bitand","bitor","bitxor","bitnot","bitshiftl","bitshiftr","bitshiftr0","print-stacktrace","cell","ref","++","--","time"]]]

},{}],2:[function(_dereq_,module,exports){
module.exports = {
    ps: ps, choice: choice, range: range, action: action, sequence: sequence, join_action: join_action, negate: negate, repeat0: repeat0, optional: optional, repeat1: repeat1, wsequence: wsequence, whitespace: whitespace, ch: ch, butnot: butnot
} /* browserify adaptation for wat, rest of code below unchanged */

// Copyright (C) 2007 Chris Double.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// DEVELOPERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

function foldl(f, initial, seq) {
    for(var i=0; i< seq.length; ++i)
        initial = f(initial, seq[i]);
    return initial;
}

var memoize = true;

function ParseState(input, index) {
    this.input = input;
    this.index = index || 0;
    this.length = input.length - this.index;
    this.cache = { };
    return this;
}

ParseState.prototype.from = function(index) {
    var r = new ParseState(this.input, this.index + index);
    r.cache = this.cache;
    r.length = this.length - index;
    return r;
}

ParseState.prototype.substring = function(start, end) {
    return this.input.substring(start + this.index, (end || this.length) + this.index);
}

ParseState.prototype.trimLeft = function() {
    var s = this.substring(0);
    var m = s.match(/^\s+/);
    return m ? this.from(m[0].length) : this;
}

ParseState.prototype.at = function(index) {
    return this.input.charAt(this.index + index);
}

ParseState.prototype.toString = function() {
    return 'PS"' + this.substring(0) + '"';
}

ParseState.prototype.getCached = function(pid) {
    if(!memoize)
        return false;

    var p = this.cache[pid];
    if(p)
        return p[this.index];
    else
        return false;
}

ParseState.prototype.putCached = function(pid, cached) {
    if(!memoize)
        return false;

    var p = this.cache[pid];
    if(p)
        p[this.index] = cached;
    else {
        p = this.cache[pid] = { };
        p[this.index] = cached;
    }
}

function ps(str) {
    return new ParseState(str);
}

// 'r' is the remaining string to be parsed.
// 'matched' is the portion of the string that
// was successfully matched by the parser.
// 'ast' is the AST returned by the successfull parse.
function make_result(r, matched, ast) {
        return { remaining: r, matched: matched, ast: ast };
}

var parser_id = 0;

// 'token' is a parser combinator that given a string, returns a parser
// that parses that string value. The AST contains the string that was parsed.
function token(s) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var r = state.length >= s.length && state.substring(0,s.length) == s;
        if(r)
            cached = { remaining: state.from(s.length), matched: s, ast: s };
        else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Like 'token' but for a single character. Returns a parser that given a string
// containing a single character, parses that character value.
function ch(c) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = state.length >= 1 && state.at(0) == c;
        if(r)
            cached = { remaining: state.from(1), matched: c, ast: c };
        else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    };
}

// 'range' is a parser combinator that returns a single character parser
// (similar to 'ch'). It parses single characters that are in the inclusive
// range of the 'lower' and 'upper' bounds ("a" to "z" for example).
function range(lower, upper) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        if(state.length < 1)
            cached = false;
        else {
            var ch = state.at(0);
            if(ch >= lower && ch <= upper)
                cached = { remaining: state.from(1), matched: ch, ast: ch };
            else
                cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Helper function to convert string literals to token parsers
// and perform other implicit parser conversions.
function toParser(p) {
    return (typeof(p) == "string") ? token(p) : p;
}

// Parser combinator that returns a parser that
// skips whitespace before applying parser.
function whitespace(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        cached = p(state.trimLeft());
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Parser combinator that passes the AST generated from the parser 'p'
// to the function 'f'. The result of 'f' is used as the AST in the result.
function action(p, f) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var x = p(state);
        if(x) {
            x.ast = f(x.ast);
            cached = x;
        }
        else {
            cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Given a parser that produces an array as an ast, returns a
// parser that produces an ast with the array joined by a separator.
function join_action(p, sep) {
    return action(p, function(ast) { return ast.join(sep); });
}

// Given an ast of the form [ Expression, [ a, b, ...] ], convert to
// [ [ [ Expression [ a ] ] b ] ... ]
// This is used for handling left recursive entries in the grammar. e.g.
// MemberExpression:
//   PrimaryExpression
//   FunctionExpression
//   MemberExpression [ Expression ]
//   MemberExpression . Identifier
//   new MemberExpression Arguments
function left_factor(ast) {
    return foldl(function(v, action) {
                     return [ v, action ];
                 },
                 ast[0],
                 ast[1]);
}

// Return a parser that left factors the ast result of the original
// parser.
function left_factor_action(p) {
    return action(p, left_factor);
}

// 'negate' will negate a single character parser. So given 'ch("a")' it will successfully
// parse any character except for 'a'. Or 'negate(range("a", "z"))' will successfully parse
// anything except the lowercase characters a-z.
function negate(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        if(state.length >= 1) {
            var r = p(state);
            if(!r)
                cached =  make_result(state.from(1), state.at(0), state.at(0));
            else
                cached = false;
        }
        else {
            cached = false;
        }
        savedState.putCached(pid, cached);
        return cached;
    };
}

// 'end_p' is a parser that is successful if the input string is empty (ie. end of parse).
function end_p(state) {
    if(state.length == 0)
        return make_result(state, undefined, undefined);
    else
        return false;
}

// 'nothing_p' is a parser that always fails.
function nothing_p(state) {
    return false;
}

// 'sequence' is a parser combinator that processes a number of parsers in sequence.
// It can take any number of arguments, each one being a parser. The parser that 'sequence'
// returns succeeds if all the parsers in the sequence succeeds. It fails if any of them fail.
function sequence() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i]));
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }

        var ast = [];
        var matched = "";
        var i;
        for(i=0; i< parsers.length; ++i) {
            var parser = parsers[i];
            var result = parser(state);
            if(result) {
                state = result.remaining;
                if(result.ast != undefined) {
                    ast.push(result.ast);
                    matched = matched + result.matched;
                }
            }
            else {
                break;
            }
        }
        if(i == parsers.length) {
            cached = make_result(state, matched, ast);
        }
        else
            cached = false;
        savedState.putCached(pid, cached);
        return cached;
    };
}

// Like sequence, but ignores whitespace between individual parsers.
function wsequence() {
    var parsers = [];
    for(var i=0; i < arguments.length; ++i) {
        parsers.push(whitespace(toParser(arguments[i])));
    }
    return sequence.apply(null, parsers);
}

// 'choice' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that succeeds results in a
// successfull parse. It fails if all parsers fail.
function choice() {
    var parsers = [];
    for(var i = 0; i < arguments.length; ++i)
        parsers.push(toParser(arguments[i]));
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }
        var i;
        for(i=0; i< parsers.length; ++i) {
            var parser=parsers[i];
            var result = parser(state);
            if(result) {
                break;
            }
        }
        if(i == parsers.length)
            cached = false;
        else
            cached = result;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// 'butnot' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not, or
// 'p1' matches and the matched text is longer that p2's.
// Useful for things like: butnot(IdentifierName, ReservedWord)
function butnot(p1,p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a but not b. if both match and b's matched text is shorter
    // than a's, a failed match is made
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var br = p2(state);
        if(!br) {
            cached = p1(state);
        } else {
            var ar = p1(state);

            if (ar) {
              if(ar.matched.length > br.matched.length)
                  cached = ar;
              else
                  cached = false;
            }
            else {
              cached = false;
            }
        }
        savedState.putCached(pid, cached);
        return cached;
    }
}

// 'difference' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not. If
// both match then if p2's matched text is shorter than p1's it is successfull.
function difference(p1,p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a but not b. if both match and b's matched text is shorter
    // than a's, a successfull match is made
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var br = p2(state);
        if(!br) {
            cached = p1(state);
        } else {
            var ar = p1(state);
            if(ar.matched.length >= br.matched.length)
                cached = br;
            else
                cached = ar;
        }
        savedState.putCached(pid, cached);
        return cached;
    }
}


// 'xor' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' or 'p2' match but fails if
// they both match.
function xor(p1, p2) {
    var p1 = toParser(p1);
    var p2 = toParser(p2);
    var pid = parser_id++;

    // match a or b but not both
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var ar = p1(state);
        var br = p2(state);
        if(ar && br)
            cached = false;
        else
            cached = ar || br;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that takes one parser. It returns a parser that
// looks for zero or more matches of the original parser.
function repeat0(p) {
    var p = toParser(p);
    var pid = parser_id++;

    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached) {
            return cached;
        }

        var ast = [];
        var matched = "";
        var result;
        while(result = p(state)) {
            ast.push(result.ast);
            matched = matched + result.matched;
            if(result.remaining.index == state.index)
                break;
            state = result.remaining;
        }
        cached = make_result(state, matched, ast);
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that takes one parser. It returns a parser that
// looks for one or more matches of the original parser.
function repeat1(p) {
    var p = toParser(p);
    var pid = parser_id++;

    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;

        var ast = [];
        var matched = "";
        var result= p(state);
        if(!result)
            cached = false;
        else {
            while(result) {
                ast.push(result.ast);
                matched = matched + result.matched;
                if(result.remaining.index == state.index)
                    break;
                state = result.remaining;
                result = p(state);
            }
            cached = make_result(state, matched, ast);
        }
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that takes one parser. It returns a parser that
// matches zero or one matches of the original parser.
function optional(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r || make_result(state, "", false);
        savedState.putCached(pid, cached);
        return cached;
    }
}

// A parser combinator that ensures that the given parser succeeds but
// ignores its result. This can be useful for parsing literals that you
// don't want to appear in the ast. eg:
// sequence(expect("("), Number, expect(")")) => ast: Number
function expect(p) {
    return action(p, function(ast) { return undefined; });
}

function chain(p, s, f) {
    var p = toParser(p);

    return action(sequence(p, repeat0(action(sequence(s, p), f))),
                  function(ast) { return [ast[0]].concat(ast[1]); });
}

// A parser combinator to do left chaining and evaluation. Like 'chain', it expects a parser
// for an item and for a seperator. The seperator parser's AST result should be a function
// of the form: function(lhs,rhs) { return x; }
// Where 'x' is the result of applying some operation to the lhs and rhs AST's from the item
// parser.
function chainl(p, s) {
    var p = toParser(p);
    return action(sequence(p, repeat0(sequence(s, p))),
                  function(ast) {
                      return foldl(function(v, action) { return action[0](v, action[1]); }, ast[0], ast[1]);
                  });
}

// A parser combinator that returns a parser that matches lists of things. The parser to
// match the list item and the parser to match the seperator need to
// be provided. The AST is the array of matched items.
function list(p, s) {
    return chain(p, s, function(ast) { return ast[1]; });
}

// Like list, but ignores whitespace between individual parsers.
function wlist() {
    var parsers = [];
    for(var i=0; i < arguments.length; ++i) {
        parsers.push(whitespace(arguments[i]));
    }
    return list.apply(null, parsers);
}

// A parser that always returns a zero length match
function epsilon_p(state) {
    return make_result(state, "", undefined);
}

// Allows attaching of a function anywhere in the grammer. If the function returns
// true then parse succeeds otherwise it fails. Can be used for testing if a symbol
// is in the symbol table, etc.
function semantic(f) {
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        cached = f() ? make_result(state, "", undefined) : false;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// The and predicate asserts that a certain conditional
// syntax is satisfied before evaluating another production. Eg:
// sequence(and("0"), oct_p)
// (if a leading zero, then parse octal)
// It succeeds if 'p' succeeds and fails if 'p' fails. It never
// consume any input however, and doesn't put anything in the resulting
// AST.
function and(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        var r = p(state);
        cached = r ? make_result(state, "", undefined) : false;
        savedState.putCached(pid, cached);
        return cached;
    }
}

// The opposite of 'and'. It fails if 'p' succeeds and succeeds if
// 'p' fails. It never consumes any input. This combined with 'and' can
// be used for 'lookahead' and disambiguation of cases.
//
// Compare:
// sequence("a",choice("+","++"),"b")
//   parses a+b
//   but not a++b because the + matches the first part and peg's don't
//   backtrack to other choice options if they succeed but later things fail.
//
// sequence("a",choice(sequence("+", not("+")),"++"),"b")
//    parses a+b
//    parses a++b
//
function not(p) {
    var p = toParser(p);
    var pid = parser_id++;
    return function(state) {
        var savedState = state;
        var cached = savedState.getCached(pid);
        if(cached)
            return cached;
        cached = p(state) ? false : make_result(state, "", undefined);
        savedState.putCached(pid, cached);
        return cached;
    }
}

},{}],3:[function(_dereq_,module,exports){
var jsparse = _dereq_("./jsparse.js");
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
    else if (str[0] === "@") { return ["js-invoker", ["wat-string", str.substring(1)]]; }
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

},{"./jsparse.js":2}],4:[function(_dereq_,module,exports){
(function (global){
// Wat VM by Manuel Simoni (msimoni@gmail.com)
module.exports = function WatVM(user_boot_bytecode, parser) {
    /* Continuations */
    function Continuation(fun, next, dbg, e) {
        this.fun = fun; this.next = next; this.dbg = dbg; this.e = e; }
    function isContinuation(x) { return x instanceof Continuation; }
    function Capture(prompt, handler) {
        this.prompt = prompt; this.handler = handler; this.k = null; }
    function isCapture(x) { return x instanceof Capture; }
    function captureFrame(capture, fun, dbg, e) {
        capture.k = new Continuation(fun, capture.k, dbg, e); }
    function continueFrame(k, f) {
        return k.fun(k.next, f); }
    /* Evaluation Core */
    function evaluate(e, k, f, x) {
        if (x && x.wat_eval) return x.wat_eval(e, k, f); else return x; }
    function Sym(name) { this.name = name; }
    function sym(name) { return new Sym(name); }
    Sym.prototype.wat_eval = function(e, k, f) { return lookup(e, this.name); };
    function Cons(car, cdr) { this.car = car; this.cdr = cdr; }
    Cons.prototype.wat_eval = function(e, k, f) {
        if (isContinuation(k)) {
            var op = continueFrame(k, f);
        } else {
            var op = evaluate(e, null, null, car(this));
        }
        if (isCapture(op)) {
            var that = this;
            captureFrame(op, function(k, f) { return that.wat_eval(e, k, f); }, this, e);
            return op;
        }
        return combine(e, null, null, op, cdr(this));
    };
    /* Operative & Applicative Combiners */
    function combine(e, k, f, cmb, o) {
        if (cmb && cmb.wat_combine) return cmb.wat_combine(e, k, f, o);
        else if (cmb instanceof Function) return jswrap(cmb).wat_combine(e, k, f, o);
        else return error("not a combiner: " + to_string(cmb)); }
    function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
    function Apv(cmb) { this.cmb = cmb; }
    function wrap(cmb) { return new Apv(cmb); };
    function unwrap(apv) { return apv instanceof Apv ? apv.cmb : error("cannot unwrap: " + apv); }
    Opv.prototype.wat_combine = function(e, k, f, o) {
        var xe = make_env(this.e); 
        var pCap = bind(xe, this.p, o);
        if (isCapture(pCap)) return pCap;
        var epCap = bind(xe, this.ep, e);
        if (isCapture(epCap)) return epCap;
        return evaluate(xe, k, f, this.x);
    };
    Apv.prototype.wat_combine = function(e, k, f, o) {
        if (isContinuation(k)) {
            var args = continueFrame(k, f);
        } else {
            var args = evalArgs(e, null, null, o, NIL);
        }
        if (isCapture(args)) {
            var that = this;
            captureFrame(args, function(k, f) { return that.wat_combine(e, k, f, o); }, cons(this, o), e);
            return args;
        }
        return this.cmb.wat_combine(e, null, null, args);
    };
    function evalArgs(e, k, f, todo, done) {
        if (todo === NIL) { return reverse_list(done); }
        if (isContinuation(k)) {
            var arg = continueFrame(k, f);
        } else {
            var arg = evaluate(e, null, null, car(todo));
        }
        if (isCapture(arg)) {
            captureFrame(arg, function(k, f) { return evalArgs(e, k, f, todo, done); }, car(todo), e);
            return arg;
        }
        return evalArgs(e, null, null, cdr(todo), cons(arg, done));
    }
    /* Built-in Combiners */
    function Vau() {}; function Def() {}; function Eval() {};
    Vau.prototype.wat_combine = function(e, k, f, o) {
        return new Opv(elt(o, 0), elt(o, 1), elt(o, 2), e); };
    Def.prototype.wat_combine = function self(e, k, f, o) {
        var lhs = elt(o, 0); if (isCapture(lhs)) return lhs;
        var rhs = elt(o, 1); if (isCapture(rhs)) return rhs;
        if (isContinuation(k)) {
            var val = continueFrame(k, f);
        } else {
            var val = evaluate(e, null, null, rhs);
        }
        if (isCapture(val)) {
            captureFrame(val, function(k, f) { return self(e, k, f, o); }, rhs, e);
            return val;
        }
        return bind(e, lhs, val);
    }
    Eval.prototype.wat_combine = function(e, k, f, o) {
        var x = elt(o, 0); if (isCapture(x)) return x;
        var e = elt(o, 1); if (isCapture(e)) return e;
        return evaluate(e, k, f, x); };
    /* First-order Control */
    function Begin() {}; function If() {}; function Loop() {}
    function Catch() {}; function Finally() {}
    Begin.prototype.wat_combine = function(e, k, f, o) {
        if (o === NIL) return null; else return begin(e, k, f, o); };
    function begin(e, k, f, xs) {
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = evaluate(e, null, null, car(xs));
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return begin(e, k, f, xs); }, car(xs), e);
            return res;
        }
        var kdr = cdr(xs);
        if (kdr === NIL) return res; else return begin(e, null, null, kdr);
    }
    If.prototype.wat_combine = function self(e, k, f, o) {
        if (isContinuation(k)) {
            var test = continueFrame(k, f);
        } else {
            var test = evaluate(e, null, null, elt(o, 0));
        }
        if (isCapture(test)) {
            captureFrame(test, function(k, f) { return self(e, k, f, o); }, elt(o, 0), e);
            return test;
        }
        return evaluate(e, null, null, test ? elt(o, 1) : elt(o, 2));
    };
    Loop.prototype.wat_combine = function self(e, k, f, o) {
        var first = true; // only continue once
        while (true) {
            if (first && isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, elt(o, 0));
            }
            first = false;
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, elt(o, 0), e);
                return res;
            }
        }
    };
    Catch.prototype.wat_combine = function self(e, k, f, o) {
        var x = elt(o, 0);
        var handler = elt(o, 1);
        try {
            if (isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, x);
            }
        } catch(exc) {
            // unwrap handler to prevent eval if exc is sym or cons
            var res = combine(e, null, null, unwrap(handler), list(exc));
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return self(e, k, f, o); }, x, e);
            return res;
        } else {
            return res;
        }
    };
    Finally.prototype.wat_combine = function self(e, k, f, o) {
        var prot = elt(o, 0);
        var cleanup = elt(o, 1);
        try {
            if (isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, prot);
            }
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, prot, e);
            }
        } finally {
            if (isCapture(res)) {
                return res;
            } else {
                return doCleanup(e, null, null, cleanup, res);
            }
        }
    };
    function doCleanup(e, k, f, cleanup, res) {
        if (isContinuation(k)) {
            var fres = continueFrame(k, f);
        } else {
            var fres = evaluate(e, null, null, cleanup);
        }
        if (isCapture(fres)) {
            captureFrame(fres, function(k, f) { return doCleanup(e, k, f, cleanup, res); }, cleanup, e);
            return fres;
        } else {
            return res;
        }
    }
    /* Delimited Control */
    function PushPrompt() {}; function TakeSubcont() {}; function PushSubcont() {}; function PushPromptSubcont() {}
    PushPrompt.prototype.wat_combine = function self(e, k, f, o) {
        var prompt = elt(o, 0);
        var x = elt(o, 1);
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = evaluate(e, null, null, x);
        }
        if (isCapture(res)) {
            if (res.prompt === prompt) {
                var continuation = res.k;
                var handler = res.handler;
                return combine(e, null, null, handler, cons(continuation, NIL));
            } else {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, x, e);
                return res;
            }
        } else {
            return res;
        }
    };
    TakeSubcont.prototype.wat_combine = function(e, k, f, o) {
        var prompt = elt(o, 0);
        var handler = elt(o, 1);
        var cap = new Capture(prompt, handler);
        captureFrame(cap, function(k, thef) { return combine(e, null, null, thef, NIL); }, this, e);
        return cap;
    };
    PushSubcont.prototype.wat_combine = function self(e, k, f, o) {
        var thek = elt(o, 0);
        var thef = elt(o, 1);
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = continueFrame(thek, thef);
        }
        if (isCapture(res)) {
            captureFrame(res, function(k, f) { return self(e, k, f, o); }, thef, e);
            return res;
        } else {
            return res;
        }
    };
    PushPromptSubcont.prototype.wat_combine = function self(e, k, f, o) {
        var prompt = elt(o, 0);
        var thek = elt(o, 1);
        var thef = elt(o, 2);
        if (isContinuation(k)) {
            var res = continueFrame(k, f);
        } else {
            var res = continueFrame(thek, thef);
        }
        if (isCapture(res)) {
            if (res.prompt === prompt) {
                var continuation = res.k;
                var handler = res.handler;
                return combine(e, null, null, handler, cons(continuation, NIL));
            } else {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, thef, e);
                return res;
            }
        } else {
            return res;
        }
    };
    /* Dynamic Variables */
    function DV(val) { this.val = val; }
    function DNew() {}; function DRef() {}; function DLet() {}
    DNew.prototype.wat_combine = function(e, k, f, o) { return new DV(elt(o, 0)); };
    DRef.prototype.wat_combine = function(e, k, f, o) { return elt(o, 0).val; };
    DLet.prototype.wat_combine = function self(e, k, f, o) {
        var dv = elt(o, 0);
        var val = elt(o, 1);
        var x = elt(o, 2);
        var oldVal = dv.val;
        dv.val = val;
        try {
            if (isContinuation(k)) {
                var res = continueFrame(k, f);
            } else {
                var res = evaluate(e, null, null, x);
            }
            if (isCapture(res)) {
                captureFrame(res, function(k, f) { return self(e, k, f, o); }, x, e);
                return res;
            } else {
                return res;
            }
        } finally {
            dv.val = oldVal;
        }
    };
    /* Objects */
    function Nil() {}; var NIL = new Nil();
    function Ign() {}; var IGN = new Ign();
    function cons(car, cdr) { return new Cons(car, cdr); }
    function car(cons) {
        if (cons instanceof Cons) return cons.car; else return error("not a cons: " + to_string(cons)); }
    function cdr(cons) {
        if (cons instanceof Cons) return cons.cdr; else return error("not a cons: " + to_string(cons)); }
    function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
    function sym_name(sym) { return sym.name; }
    function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); this.parent = parent; }
    function make_env(parent) { return new Env(parent); }
    function lookup(e, name) {
        if (name in e.bindings) return e.bindings[name];
        else return error("unbound: " + name); }
    function bind(e, lhs, rhs) { 
        if (lhs.wat_match) return lhs.wat_match(e, rhs); else return error("cannot match against: " + lhs); }
    Sym.prototype.wat_match = function(e, rhs) {
        return e.bindings[this.name] = rhs; }
    Cons.prototype.wat_match = function(e, rhs) {
        var carCap = car(this).wat_match(e, car(rhs));
        if (isCapture(carCap)) return carCap;
        var cdrCap = cdr(this).wat_match(e, cdr(rhs));
        if (isCapture(cdrCap)) return cdrCap; };
    Nil.prototype.wat_match = function(e, rhs) {
        if (rhs !== NIL) return error("NIL expected, but got: " + to_string(rhs)); };
    Ign.prototype.wat_match = function(e, rhs) {};
    /* Setter - you are not expected to understand this - immediately */
    var SETTER = jswrap(function setter(obj) { return obj.wat_setter; });
    SETTER.wat_setter = jswrap(function(new_setter, obj) { obj.wat_setter = new_setter; });
    /* Error handling */
    var ROOT_PROMPT = {};
    function push_root_prompt(x) { return list(new PushPrompt(), ROOT_PROMPT, x); }
    function error(err) {
        var print_stacktrace = environment.bindings["user-break"];
        if (print_stacktrace !== undefined) {
            return combine(environment, null, null, print_stacktrace, list(err));
        } else { throw err; } }
    /* Utilities */
    function list() {
        return array_to_list(Array.prototype.slice.call(arguments)); }
    function list_star() {
        var len = arguments.length; var c = len >= 1 ? arguments[len-1] : NIL;
        for (var i = len-1; i > 0; i--) c = cons(arguments[i - 1], c); return c; }
    function array_to_list(array, end) {
        var c = end ? end : NIL;
        for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c); return c; }
    function list_to_array(c) {
        var res = []; while(c !== NIL) { res.push(car(c)); c = cdr(c); } return res; }
    function reverse_list(list) {
        var res = NIL; while(list !== NIL) { res = cons(car(list), res); list = cdr(list); } return res; }
    var js_types = ["Array", "Boolean", "Date", "Function", "Number", "Object", "RegExp", "String"];
    function is_type(obj, type_obj, type_name) {
        if (!type_obj) return error("type is undefined");
        if (js_types.indexOf(type_name) === -1) { return obj instanceof type_obj; }
        else { return toString.call(obj) === "[object " + type_name + "]"; } }
    /* Bytecode parser */
    function parse_bytecode(obj) {
        switch(Object.prototype.toString.call(obj)) {
        case "[object String]": return obj === "#ignore" ? IGN : sym(obj);
        case "[object Array]": return parse_bytecode_array(obj);
        default: return obj; } }
    function parse_bytecode_array(arr) {
        if ((arr.length == 2) && arr[0] === "wat-string") { return arr[1]; }
        var i = arr.indexOf(".");
        if (i === -1) return array_to_list(arr.map(parse_bytecode));
        else { var front = arr.slice(0, i);
               return array_to_list(front.map(parse_bytecode), parse_bytecode(arr[i + 1])); } }
    /* JSNI */
    function JSFun(jsfun) {
        if (Object.prototype.toString.call(jsfun) !== "[object Function]") return error("no fun");
        this.jsfun = jsfun; }
    JSFun.prototype.wat_combine = function(e, k, f, o) {
        return this.jsfun.apply(null, list_to_array(o)); };
    function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
    function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")); }
    function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")); }
    function js_invoker(method_name) {
        return jswrap(function() {
            if (arguments.length < 1) return error("invoker called with wrong args: " + arguments);
            if (!method_name) return error("method name is null/undefined");
            var rcv = arguments[0];
            if (!rcv) return error("receiver is null/undefined");
            var method = rcv[method_name];
            if (!method) return error("method not found: " + method_name + " in: " + to_string(rcv));
            return method.apply(rcv, Array.prototype.slice.call(arguments, 1)); }); }
    function js_getter(prop_name) {
        var getter = jswrap(function() {
            if (arguments.length !== 1) return error("getter called with wrong args: " + arguments);
            var rcv = arguments[0];
            if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name];
            else return error("can't get " + prop_name + " of " + rcv); });
        getter.wat_setter = js_setter(prop_name); return getter; }
    function js_setter(prop_name) {
        return jswrap(function() {
            if (arguments.length !== 2) return error("setter called with wrong args: " + arguments);
            var rcv = arguments[1];
            if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name] = arguments[0];
            else return error("can't set " + prop_name + " of " + rcv); }); }
    function make_prototype(name) {
        var prop_names = Array.prototype.slice.call(arguments, 1);
        var param_names = prop_names.join(",");
        var param_inits = prop_names.map(function(prop_name) {
            return "this." + prop_name + "=" + prop_name + ";"; }).join("");
        return eval("(function " + name + "(" + param_names + "){" + param_inits + "})"); }
    function jsnew(ctor) {
        var factoryFunction = ctor.bind.apply(ctor, arguments);
        return new factoryFunction(); }
    function js_function(cmb) {
        return function() {
            var args = cons(this, array_to_list(Array.prototype.slice.call(arguments)));
            return combine(null, null, null, cmb, args); } }
    var JS_GLOBAL = jswrap(function(name) { return eval(name); });
    JS_GLOBAL.wat_setter = jswrap(function(new_val, name) { global[name] = new_val; });
    /* Stringification */
    function to_string(obj) {
        if (toString.call(obj) === "[object String]") return JSON.stringify(obj);
        else if ((obj !== null) && (obj !== undefined)) return obj.toString();
        else return Object.prototype.toString.call(obj); }
    Nil.prototype.toString = function() { return "()"; };
    Ign.prototype.toString = function() { return "#ignore"; };
    Sym.prototype.toString = function() { return this.name; };
    Cons.prototype.toString = function() { return "(" + cons_to_string(this) + ")" };
    function cons_to_string(c) {
        if (cdr(c) === NIL) return to_string(car(c));
        else if (cdr(c) instanceof Cons) { return to_string(car(c)) + " " + cons_to_string(cdr(c)); }
        else return to_string(car(c)) + " . " + to_string(cdr(c)); }
    Apv.prototype.toString = function() { return "[Apv " + to_string(this.cmb) + "]"; };
    Opv.prototype.toString = function() {
        return "[Opv " + to_string(this.p) + " " + to_string(this.ep) + " " + to_string(this.x) + "]"; };
    Vau.prototype.toString = function() { return "vm-vau"; };
    Def.prototype.toString = function() { return "vm-def"; };
    Eval.prototype.toString = function() { return "vm-eval"; };
    Begin.prototype.toString = function() { return "vm-begin"; };
    If.prototype.toString = function() { return "vm-if"; };
    Loop.prototype.toString = function() { return "vm-loop"; };
    Catch.prototype.toString = function() { return "vm-catch"; };
    Finally.prototype.toString = function() { return "vm-finally"; };
    DLet.prototype.toString = function() { return "vm-dlet"; }
    DNew.prototype.toString = function() { return "vm-dnew"; }
    DRef.prototype.toString = function() { return "vm-dref"; }
    PushPrompt.prototype.toString = function() { return "vm-push-prompt"; }
    TakeSubcont.prototype.toString = function() { return "vm-take-subcont"; }
    PushSubcont.prototype.toString = function() { return "vm-push-subcont"; }
    PushPromptSubcont.prototype.toString = function() { return "vm-push-prompt-subcont"; }
    JSFun.prototype.toString = function() { return "[JSFun " + this.jsfun.toString() + "]"; };
    /* Bootstrap */
    var boot_bytecode =
        ["vm-begin",
         // Basics
         ["vm-def", "vm-vau", new Vau()],
         ["vm-def", "vm-eval", wrap(new Eval())],
         ["vm-def", "vm-make-environment", jswrap(function(env) { return make_env(env); })],
         ["vm-def", "vm-wrap", jswrap(wrap)],
         ["vm-def", "vm-unwrap", jswrap(unwrap)],
         // Values
         ["vm-def", "vm-cons", jswrap(cons)],
         ["vm-def", "vm-cons?", jswrap(function(obj) { return obj instanceof Cons; })],
         ["vm-def", "vm-nil?", jswrap(function(obj) { return obj === NIL; })],
         ["vm-def", "vm-symbol?", jswrap(function(obj) { return obj instanceof Sym; })],
         ["vm-def", "vm-symbol-name", jswrap(sym_name)],
         // First-order Control
         ["vm-def", "vm-if", new If()],
         ["vm-def", "vm-loop", new Loop()],
         ["vm-def", "vm-throw", jswrap(function(err) { throw err; })],
         ["vm-def", "vm-catch", new Catch()],
         ["vm-def", "vm-finally", new Finally()],
         // Delimited Control
         ["vm-def", "vm-push-prompt", new PushPrompt()],
         ["vm-def", "vm-take-subcont", wrap(new TakeSubcont())],
         ["vm-def", "vm-push-subcont", wrap(new PushSubcont())],
         ["vm-def", "vm-push-prompt-subcont", wrap(new PushPromptSubcont())],
         // Dynamically-scoped Variables
         ["vm-def", "vm-dnew", wrap(new DNew())],
         ["vm-def", "vm-dlet", new DLet()],
         ["vm-def", "vm-dref", wrap(new DRef())],
         // Setters
         ["vm-def", "vm-setter", SETTER],
         // Errors
         ["vm-def", "vm-root-prompt", ROOT_PROMPT],
         ["vm-def", "vm-error", jswrap(error)],
         // JS Interface
         ["vm-def", "vm-js-wrap", jswrap(jswrap)],
         ["vm-def", "vm-js-unop", jswrap(js_unop)],
         ["vm-def", "vm-js-binop", jswrap(js_binop)],
         ["vm-def", "vm-js-getter", jswrap(js_getter)],
         ["vm-def", "vm-js-setter", jswrap(js_setter)],
         ["vm-def", "vm-js-invoker", jswrap(js_invoker)],
         ["vm-def", "vm-js-function", jswrap(js_function)],
         ["vm-def", "vm-js-global", JS_GLOBAL],
         ["vm-def", "vm-js-make-object", jswrap(function() { return {}; })],
         ["vm-def", "vm-js-make-prototype", jswrap(make_prototype)],
         ["vm-def", "vm-js-new", jswrap(jsnew)],
         ["vm-def", "vm-type?", jswrap(is_type)],
         // Utilities
         ["vm-def", "vm-list-to-array", jswrap(list_to_array)],
         ["vm-def", "vm-array-to-list", jswrap(array_to_list)],
         ["vm-def", "vm-reverse-list", jswrap(reverse_list)],
         ["vm-def", "vm-list*", jswrap(list_star)],
         // User-supplied boot code; defines user environment
         ["vm-begin"].concat(user_boot_bytecode)
        ];
    var environment = make_env();
    bind(environment, sym("vm-def"), new Def());
    bind(environment, sym("vm-begin"), new Begin());
    var ms = new Date().getTime();
    var user_environment = evaluate(environment, null, null, parse_bytecode(boot_bytecode));
    if (!(user_environment instanceof Env)) throw "failed to boot Wat";
    console.log("Wat VM startup time: " + (new Date().getTime() - ms) + "ms");
    /* API */
    this.eval = function(sexp){
        if (!parser) throw "parsing not supported"; return this.exec(parser.parse_sexp(sexp)); }
    this.exec = function(bytecode) {
        var wrapped = push_root_prompt(parse_bytecode([new Begin()].concat(bytecode)));
        var res = evaluate(user_environment, null, null, wrapped);
        if (isCapture(res)) throw "prompt not found: " + res.prompt;
        return res; }
    this.call = function(fun_name) {
        return this.exec(parse_bytecode([fun_name].concat(Array.prototype.slice.call(arguments, 1)))); }
    this.get = function(var_name) { return this.exec(parse_bytecode(var_name)); }
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],5:[function(_dereq_,module,exports){
var vm = _dereq_("./vm.js");
var boot = _dereq_("./build/boot.js");
var parser = _dereq_("./parser.js");
module.exports.vm = function() { return new vm(boot.main, parser); }

},{"./build/boot.js":1,"./parser.js":3,"./vm.js":4}]},{},[5])
(5)
});