;; -*- mode: scheme -*-

  ;;;;; Test Core Language

  ;; DEF

  (provide ()
    (def (x y) (list #t #f))
    (assert (eq? x #t))
    (assert (eq? y #f))
    
    (assert (eq? (def #ign #t) #t)))

  ;; IF

  (provide ()
    (assert (eq? #t (if #t #t #f)))
    (assert (eq? #f (if #f #t #f))))

  ;; VAU

  (provide ()
    (def env (current-environment))
    (eq? #t ((vau x #ign x) #t))
    (eq? #t ((vau (x . #ign) #ign x) (list #t)))
    (eq? env ((vau #ign e e))))

  ;; EVAL

  (provide ()
    (def env (current-environment))
    (eval (list def (quote x) #t) env)
    (assert (eq? x #t))
    
    (assert (eq? (eval #t env) #t)))

  ;; WRAP

  (provide ()
    (assert (eq? #t ((wrap (vau (x) #ign x)) (not #f)))))

  ;; UNWRAP

  (provide ()
    (assert (eq? list (unwrap (wrap list)))))

  ;; EQ?

  (provide ()
    (assert (eq? #t #t))
    (assert (not (eq? #t #f)))
    (assert (not (eq? (list 1) (list 1)))))

  ;; CONS

  (provide ()
    (assert (eq? #t (car (cons #t #f))))
    (assert (eq? #f (cdr (cons #t #f)))))

  ;; MAKE-ENVIRONMENT

  (provide ()
    (def e1 (make-environment))
    (eval (list def (quote x) #t) e1)
    (eval (list def (quote y) #t) e1)
    (assert (eq? #t (eval (quote x) e1)))
    (assert (eq? #t (eval (quote y) e1)))

    (def e2 (make-environment e1))
    (assert (eq? #t (eval (quote x) e2)))
    (assert (eq? #t (eval (quote y) e2)))
    (eval (list def (quote y) #f) e2)
    (assert (eq? #f (eval (quote y) e2)))
    (assert (eq? #t (eval (quote y) e1))))

  ;; MAKE-TYPE
 
  (provide ()
    (def (type tagger untagger) (make-type))
    (assert (eq? (type-of type) (type-of (type-of #t))))
    (let ((x (list #void)))
      (eq? type (type-of (tagger x)))
      (eq? x (untagger (tagger x)))))

  ;; TYPE-OF

  (provide ()
    (assert (not (eq? (type-of () #void))))
    (assert (eq? (type-of 0) (type-of 1))))

  ;; VECTOR, VECTOR-REF

  (provide ()
    (def (a b c) (list 1 2 3))
    (def v (vector a b c))
    (assert (eq? (vector-ref v 0) a))
    (assert (eq? (vector-ref v 1) b))
    (assert (eq? (vector-ref v 2) c)))

  ;; Quotation

  (provide ()
    (assert (symbol? 'x))
    (assert (pair? '(a . b))))

  ;;;;; Test Crust Language

  ;; NULL?

  (provide ()
    (assert (null? ()))
    (assert (not (null? 12))))

  ;; BEGIN

  (provide ()
    (assert (eq? #void (begin)))
    (assert (eq? #t (begin (eq? #t #t))))
    (assert (eq? #t (begin #f (eq? #t #t)))))

;; IDENTITY-HASH-CODE

(provide ()
  (assert (not (eq? (identity-hash-code "foo") (identity-hash-code "bar")))))

;; DEFINE-RECORD-TYPE

(provide ()
  (define-record-type pare
    (kons kar kdr)
    pare?
    (kar kar set-kar!)
    (kdr kdr set-kdr!))
  (define p (kons 1 2))
  (assert (num= 1 (kar p)))
  (assert (num= 2 (kdr p)))
  (set-kar! p 3)
  (set-kdr! p 4)
  (assert (num= 3 (kar p)))
  (assert (num= 4 (kdr p)))
  (assert (pare? p))
  (assert (eq? #f (pare? 12))))

;; BOUND?

(provide ()
  (assert (eq? #f (bound? 'x (current-environment))))
  (assert (eq? #f (bound? 'y (current-environment))))
  (define x 1)
  (assert (eq? #t (bound? 'x (current-environment))))
  (assert (eq? #f (bound? 'y (current-environment))))
)

;; Hashtables

(provide ()
  (define ht (make-hashtable identity-hash-code eq?))
  (define key "key")
  (hashtable-put! ht key 12)
  (assert (num= 12 (hashtable-get ht key)))
  (assert (num= 14 (hashtable-get ht "another-key" 14)))
;  (hashtable-put! ht key 16)
;  (assert (num= 16 (hashtable-get ht key)))
)

;; Conversions

(assert (num= 1 (string->number (symbol->string (string->symbol (number->string 1))))))

;; Generics

(provide ()
  (define-generic (foo obj x) 12)
  (assert (num= (foo #void #void) 12))
  (define-method (foo (self String) x) (+ x 5))
  (assert (num= (foo #void #void) 12))
  (assert (num= (foo "blah" (+ 1 1)) 7)))

;; Generic Equality

(provide ()
  (assert (= 1 1))
  (assert (= 'foo 'foo))
  (assert (not (= 1 2)))
  (assert (not (= 1 "foo")))
  (assert (not (= "bar" "foo")))
  (assert (not (= 'bar 'foo)))
)

