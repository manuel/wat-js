;; -*- mode: scheme -*-

(define-module wat-manual ()

  (import man (document section para render))
  
  (document top "Wat Reference Manual")
  
  (section (top intro) "Introduction"
    (para "This manual documents the Wat Lisp interpreter for
JavaScript.  It is intended as a reference for knowledgeable
programmers and implementors."))

  (section (top scope) "Combiners and Bindings")
  
  (section (top first-order-control) "First-order Control")
  
  (section (top higher-order-control) "Higher-order Control")
  
  (section (top constants) "Constants")
  
  (log (render top))
  
)
