;; -*- scheme -*-

;; This file implements core Virtua forms using Wat VM intrinsics that
;; are not valid Virtua.

(wat-define list* wat-list*)

(wat-define list
  (wat-wrap (wat-fexpr x _ x)))

(wat-define wat-fexpr
  ((wat-wrap (wat-fexpr (wat-fexpr) _
               (wat-fexpr (params env-param . body) env
                 (wat-eval (list wat-fexpr params env-param
                                 (wat-cons wat-begin body))
                           env))))
   wat-fexpr))

(wat-define lambda
  (wat-macro (wat-fexpr (params . body) env
               (list wat-wrap
                     (list wat-eval
                           (list* wat-fexpr params _ body)
                           env)))))

(wat-define define-macro
  (wat-macro (wat-fexpr (name params . body) _
               (list wat-define name
                     (list wat-macro
                           (list* wat-fexpr params _ body))))))

(wat-define assert
  (wat-fexpr (expr) env
    (wat-if (wat-eval expr env)
            +wat-void+
            (wat-fail expr))))
