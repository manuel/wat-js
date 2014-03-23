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

  (section (top intro) "Design Notes"
    (para "Wat is inspired by the family of compact and clean Lisps
and Lisp-like languages that appeared from the early 1990s: ISLisp,
Dylan, Goo, ScriptX, NewtonScript, Cecil, Self."))

  (log (render top))
  
)
