;; -*- mode: scheme -*-

(provide (read)

(display "Browser")

(define *document* (js-global "document"))
(define getElementById 
  (let ((m (js-method "getElementById")))
    (lambda (str) (m *document* (to-js str)))))

(define getElementById 
  (let ((m (js-method "getElementById")))
    (lambda (str) (m *document* (to-js str)))))

(define (read-input)
  (list* 'begin (read-from-string (from-js (js-prop (getElementById "input") "value")))))

(define env (current-environment))
(define *read-k* #void)

(define (read)
  (take-sub-cont *top-level* k
    (set! env *read-k* k)))

(js-set-prop! (getElementById "button")
	      "onclick"
	      (js-callback (lambda #ign (push-prompt *top-level* (push-sub-cont *read-k* (from-js (read-input)))))))

) ; edivorp
