var VM = require("./vm.js");
var boot_bytecode = require("./build/boot.js").main;
var parser = require("./parser.js");
var vm = new VM();
vm.exec(boot_bytecode);

module.exports.vm = function() {
    return {
        "eval": function(sexp) { return vm.exec(parser.parse_sexp(sexp)); }
    };
};
