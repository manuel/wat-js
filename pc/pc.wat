;; -*- mode: scheme -*-

;; PERSONAL COMPUTING BY WAT

(provide (read display)

(define print (js-function (js-global "print")))
(define readline (js-function (js-global "readline")))

(define (display msg)
  (print (to-js msg))
  msg)

(define (read)
  (list* 'begin (read-from-string (from-js (readline)))))

) ; edivorp
