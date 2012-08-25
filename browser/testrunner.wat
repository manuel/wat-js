;; -*- scheme -*-
;; Runs examples in manual

(provide (run-tests)

  (define jquery (js-function (js-global "$")))
  (define ($ sel) (jquery (to-js sel)))
  (define-js-method contents)
  (define-js-method find)
  (define-js-method each)
  (define-js-method each)

  (define (run-tests)
    (define *manual* (contents ($ "#manual")))
    (define *examples* (find *manual* ".example"))
    (each *examples* do-example))

  (define examples-environment (current-environment))

  (define do-example
    (js-callback (lambda (#ign example)
                   (define text (from-js (js-prop example "innerText")))
                   (define forms (read-from-string text))
                   (eval (list* begin forms) (make-environment examples-environment)))))

  (run-tests)

)
