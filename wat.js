var vm = require("./vm.js");
var boot_bytecode = require("./build/boot.js").main;
var parser = require("./parser.js");
var vm = new vm(parser);
vm.exec(boot_bytecode);

module.exports.vm = function() { return vm; };
