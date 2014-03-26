(define-module wat-manual ()

  (man:document top "Wat Reference Manual")

  (man:section (top intro) "Introduction"
    (man:para "This manual documents the Wat Lisp interpreter for
JavaScript.  It is intended as a reference for knowledgeable
programmers and implementors."))

  (man:section (top scope) "Combiners and Bindings")
  
  (man:section (top first-order-control) "First-order Control")
  
  (man:section (top higher-order-control) "Higher-order Control")
  
  (man:section (top constants) "Constants")

  (man:section (top intro) "Design Notes"
    (man:para "Wat is inspired by the family of compact and clean Lisps
and Lisp-like languages that appeared from the early 1990s: ISLisp,
Dylan, Goo, ScriptX, NewtonScript, Cecil, Self."))

  (log (man:render top))
  
)
