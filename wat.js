var vm = require("./vm.js");
var boot = require("./build/boot.js");
var parser = require("./parser.js");
module.exports.vm = function() { return new vm(boot.main, parser); }
