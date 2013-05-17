// Wat Basics Library. Include wat_basics.main in your program to use.
(function(wat_basics){
wat_basics.main =
        ["begin",
         
         ["def", "compose",
          ["lambda", ["f", "g"], ["lambda", ["arg"], ["f", ["g", "arg"]]]]],

         ["def", "car", ["lambda", [["x", "#rest", "#ignore"]], "x"]],
         ["def", "cdr", ["lambda", [["#ignore", "#rest", "x"]], "x"]],
         ["def", "caar", ["compose", "car", "car"]],
         ["def", "cadr", ["compose", "car", "cdr"]],
         ["def", "cdar", ["compose", "cdr", "car"]],
         ["def", "cddr", ["compose", "cdr", "cdr"]],

         ["def", "define-macro",
          ["macro", [["name", "#rest", "params"], "#rest", "body"],
           ["list", "def", "name", ["list*", "macro", "params", "body"]]]],

         ["define-macro", ["define", "lhs", "#rest", "rhs"],
          ["if", ["cons?", "lhs"],
           ["list", "def", ["car", "lhs"], ["list*", "lambda", ["cdr", "lhs"], "rhs"]],
           ["list", "def", "lhs", ["car", "rhs"]]]],

         ["define", ["map-list", "f", "lst"],
           ["if", ["nil?", "lst"],
            [],
            ["cons", ["f", ["car", "lst"]], ["map-list", "f", ["cdr", "lst"]]]]],

         ["define-macro", ["let", "bindings", "#rest", "body"],
          ["cons",
           ["list*", "lambda", ["map-list", "car", "bindings"], "body"],
           ["map-list", "cadr", "bindings"]]],

         ["define-macro", ["let*", "bindings", "#rest", "body"],
          ["if", ["nil?", "bindings"],
           ["list*", "let", [], "body"],
           ["list", "let", ["list", ["car", "bindings"]],
            ["list*", "let*", ["cdr", "bindings"], "body"]]]],

         ["define-macro", ["where", "expr", "#rest", "bindings"],
          ["list", "let", "bindings", "expr"]],

         ["define-macro", ["where*", "expr", "#rest", "bindings"],
          ["list", "let*", "bindings", "expr"]],

         ["define", ["call-with-escape", "fun"],
          ["let", [["fresh", ["list", null]]],
           ["catch", ["fun", ["lambda", ["val"], ["throw", ["list", "fresh", "val"]]]],
            ["lambda", ["exc"],
             ["if", ["&&", ["cons?", "exc"], ["===", "fresh", ["car", "exc"]]],
              ["cadr", "exc"],
              ["throw", "exc"]]]]]],

         ["define-macro", ["let-escape", "name", "#rest", "body"],
          ["list", "call-with-escape", ["list*", "lambda", ["list", "name"], "body"]]],

         ["define", ["call-while", "test-fun", "body-fun"],
          ["let-escape", "return",
           ["loop",
            ["if", ["test-fun"],
             ["body-fun"],
             ["return", null]]]]],

         ["define-macro", ["while", "test", "#rest", "body"],
          ["list", "call-while",
           ["list", "lambda", [], "test"],
           ["list*", "lambda", [], "body"]]]

        ];
})(typeof exports === "undefined" ? this["wat_basics"] = {} : exports);
