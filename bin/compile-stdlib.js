var fs = require("fs");
var parser = require("../wat-parser.js");
console.log("module.exports.main = " + JSON.stringify(parser.parse_sexp(fs.readFileSync("stdlib.wat", "utf8"))));
