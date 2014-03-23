var fs = require("fs");
var parser = require("../parser.js");
console.log("module.exports.main = " + JSON.stringify(parser.parse_sexp(fs.readFileSync("boot.wat", "utf8"))));
