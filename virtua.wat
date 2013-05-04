;; -*- scheme -*-

;; This file implements core Virtua forms using Wat VM intrinsics that
;; are not valid Virtua.

(wat-define list* wat-list*)

(wat-define list
  (wat-wrap (wat-fexpr x _ x)))

(wat-define lambda
  (wat-macro (wat-fexpr (params . body) env
               (list wat-wrap
                     (list wat-eval
                           (list* wat-fexpr params _ body)
                           env)))))

(wat-define assert
  (wat-fexpr (expr) env
    (wat-if (wat-eval expr env)
            +wat-void+
            (wat-fail expr))))
