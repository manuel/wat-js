var vm = require("./vm.js");
var parser = require("./parser.js");
var boot = require("./build/boot.js");
module.exports.vm = function() { return new vm(boot.main, parser); }
