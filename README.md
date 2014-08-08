# Wat

### Concurrency and Metaprogramming for JS

Wat is a tiny language with powerful concurrency and metaprogramming
features for embedding in JavaScript programs.

* Delimited continuations http://www.cs.indiana.edu/~dyb/pubs/monadicDC.pdf

* Delimited dynamic binding http://okmij.org/ftp/papers/DDBinding.pdf

* First-class lexical environments and fexprs http://www.wpi.edu/Pubs/ETD/Available/etd-090110-124904/unrestricted/jshutt.pdf

* First-order control (sequential, conditional, loop, throw, catch, finally) running on JS stack

* Interop with JavaScript objects, functions, and callbacks

* JSON-based bytecode for embedding Wat code in JavaScript

Wat is developed by Manuel Simoni (msimoni@gmail.com).

### Usage

#### Browser

    <html>
    <head>
      <script type="text/javascript" src="./build/wat.js"></script>
    </head>
    <body>
      <script type="text/javascript">
        var vm = wat.vm();
        console.log(vm.eval("(+ 1 2)"));
      </script>
    </body>
    </html>

#### Node.js

    > var vm = require("./build/wat").vm();
    > vm.eval("(+ 1 2)");
    3

### More documentation upcoming!

For now, follow http://axisofeval.blogspot.com/search/label/wat and/or
https://twitter.com/msimoni for updates.
