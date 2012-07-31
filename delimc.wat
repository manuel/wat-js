;; -*- mode: scheme -*-

(provide (slot-value set-slot-value!)
  (define (slot-value tagged name)
    (let ((slots (untag tagged)))
      (eval name slots)))
  (define (set-slot-value! tagged name value)
    (let ((slots (untag tagged)))
      (eval (list define name value) slots)))
)

(provide (make-prompt push-prompt with-sub-cont push-sub-cont run-cc)

  (define *env* (current-environment))

  (define mk #void)
  (define abort #void)

  (define (run-cc th)
    (set! *env* mk (empty-chain))
    (underflow ((call/cc
		 (lambda (k)
		   (set! *env* abort k)
		   (abort th))))))

  (define prompt-type (make-type))
  (define (make-prompt) (tag prompt-type #void))

  (define-syntax push-prompt
    (vau (p . es) env
      (push-prompt* (eval p env)
        (lambda () (eval (list* begin es) env)))))

  (define-syntax push-sub-cont
    (vau (k . es) env
      (push-sub-cont* (eval k env)
        (lambda () (eval (list* begin es) env)))))

  (define (push-prompt* p th)
    (call/cc
     (lambda (k)
       (set! *env* mk (push-p p (push-seg k mk)))
       (abort th))))

  (define (with-sub-cont p f)
    (let (((subk mk*) (split-chain mk p)))
      (set! *env* mk mk*)
      (call/cc
       (lambda (k)
	 (abort (lambda () (f (push-seg k subk))))))))

  (define (push-sub-cont* subk th)
    (call/cc
     (lambda (k)
       (set! *env* mk (append-chain subk (push-seg k mk)))
       (abort th))))


  (define empty-chain-type (make-type))
  (define push-p-type (make-type))
  (define push-seg-type (make-type))

  (define (empty-chain)
    (tag empty-chain-type #void))

  (define (push-p p mk*)
    (let ((inst (tag push-p-type (make-environment))))
      (set-slot-value! inst 'p p)
      (set-slot-value! inst 'mk* mk*)
      inst))

  (define (push-seg k mk*)
    (let ((inst (tag push-seg-type (make-environment))))
      (set-slot-value! inst 'k k)
      (set-slot-value! inst 'mk* mk*)
      inst))

  (define (underflow v)
    (do-underflow mk v))

  (define-generic (do-underflow mk v))

  (define-method (do-underflow (self empty-chain-type) v)
    v)

  (define-method (do-underflow (self push-p-type) v)
    (set! *env* mk (slot-value self 'mk*))
    (underflow v))

  (define-method (do-underflow (self push-seg-type) v)
    (set! *env* mk (slot-value self 'mk*))
    ((slot-value self 'k) v))

  (define-generic (split-chain mk p))

  (define-method (split-chain (self empty-chain-type) p)
    (fail "prompt not found"))

  (define-method (split-chain (self push-p-type) p)
    (let ((p# (slot-value self 'p))
	  (sk (slot-value self 'mk*)))
         (if (eq? p p#)
              (list (empty-chain) sk)
              (let (((subk sk#) (split-chain sk p)))
		(list (push-p p# subk) sk#)))))

  (define-method (split-chain (self push-seg-type) p)
    (let ((seg (slot-value self 'k))
	  (sk (slot-value self 'mk*)))
      (let (((subk sk#) (split-chain sk p)))
	(list (push-seg seg subk) sk#))))

  (define-generic (append-chain mk mk*))

  (define-method (append-chain (self empty-chain-type) mk*)
    mk*)

  (define-method (append-chain (self push-p-type) mk*)
    (let ((p (slot-value self (quote p)))
	  (sk (slot-value self (quote mk*))))
      (push-p p (append-chain sk mk*))))

  (define-method (append-chain (self push-seg-type) mk*)
    (let ((seg (slot-value self (quote k)))
	  (sk (slot-value self (quote mk*))))
      (push-seg seg (append-chain sk mk*))))

)
