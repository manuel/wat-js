;; -*- mode: scheme -*-

;; WAT ON THE WWW

(provide (read display)

(define *document* (js-global "document"))

(define getElementById 
  (let ((m (js-method "getElementById")))
    (lambda (str) (m *document* (to-js str)))))

(define createElement
  (let ((m (js-method "createElement")))
    (lambda (name) (m *document* (to-js name)))))

(define createTextNode
  (let ((m (js-method "createTextNode")))
    (lambda (s) (m *document* (to-js s)))))

(define appendChild
  (let ((m (js-method "appendChild")))
    (lambda (e child) (m e child))))

(define (read-input)
  (let ((res (list* 'begin (read-from-string (from-js (js-prop (getElementById "input") "value"))))))
    (display (from-js (js-prop (getElementById "input") "value")))
    (js-set-prop! (getElementById "input") "value" (to-js ""))
    res))

(define *read-k* #void)

(define (read)
  (take-sub-cont *top-level* k
    (set! *read-k* k)))

(js-set-prop! (getElementById "input")
	      "onkeypress"
	      (js-callback (lambda (evt)
			     (when (= (from-js (js-prop evt "keyCode")) 13)
                      	       (push-prompt *top-level* (push-sub-cont *read-k* (read-input)))))))

(define (display msg)
  (let ((div (createElement "div")))
    (appendChild div (createTextNode (to-js msg)))
    (appendChild (getElementById "output") div)
    msg))

((js-method "focus") (getElementById "input"))

) ; edivorp
