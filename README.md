# Wat VM: Concurrency and Metaprogramming for JavaScript

Wat is an ultra-lightweight scripting language for embedding in
JavaScript programs that provides advanced concurrency and
metaprogramming tools.

* Delimited continuations http://www.cs.indiana.edu/~dyb/pubs/monadicDC.pdf

* Delimited dynamic binding http://okmij.org/ftp/papers/DDBinding.pdf

* First-class lexical environments and fexprs ftp://ftp.cs.wpi.edu/pub/techreports/pdf/05-07.pdf

* Lazily expanded hygienic macros http://people.csail.mit.edu/jaffer/CNS/interpreter-latency

* First-order control (sequential, conditional, loop, throw, catch, finally) running on JS stack

* Access to JavaScript values, functions, and globals

* JSON-based syntax for embedding Wat code in JavaScript

Wat is developed by Manuel Simoni (msimoni@gmail.com).

## Usage

### Browser

    <html>
    <head>
      <script type="text/javascript" src="wat.js"></script>
    </head>
    <body>
      <script type="text/javascript">
        var vm = new wat.VM();
        console.log(vm.run(["+", 1, 2]));
      </script>
    </body>
    </html>

### Node.js

    > var wat = require("./wat");
    > var vm = new wat.VM();
    > vm.run(["+", 1, 2]);
    3
