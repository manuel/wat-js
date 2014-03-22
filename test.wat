;; -*- mode: scheme -*-

;;;;; Wat Test Suite

;;;; Utilities

(define assert-true
  (vau (expr) e
    (unless (=== true (eval expr e))
      (error (+ "Should be true: " expr)))))

(define assert-false
  (vau (expr) e
    (unless (=== false (eval expr e))
      (error (+ "Should be false: " expr)))))

(define assert-equal
  (vau (expr1 expr2) e
    (unless (=== (eval expr1 e) (eval expr2 e))
      (error (cat "Should be equal: " expr1 " and " expr2)))))

(define assert-throws
  (vau (expr) e
    (label return
      (catch (eval expr e)
        (lambda (exc) (return)))
      (error (+ "Should throw: " expr)))))

(assert-throws (lambda))
;(assert-throws (lambda 12 12))
;(assert-throws (lambda "foo" "bar"))
(assert-throws (def))
(assert-throws (def 12))
(assert-throws (def 12 12))

(assert-equal null (begin))
(assert-equal 1 (begin 1))
(assert-equal 2 (begin 1 2))

;;;; Delimited Dynamic Binding Tests

;; adapted from 

(define-macro (test-check label expr expected)
  (list assert-equal expr expected))

(define (new-prompt) (list null))

(define (abortP p e)
  (take-subcont p ignore e))

(test-check 'test2
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p 5))
      4))
  9)

(test-check 'test3
  (let ((p (new-prompt)))
    (+ (push-prompt p (+ (abortP p 5) 6))
      4))
  9)

(test-check 'test3-1
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p (+ (abortP p 5) 6)))
      4))
  9)

(test-check 'test3-2
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
		      (v1 (abortP p 7)))
		 (+ v1 10)))))
      (+ v 20)))
  27)

'(test-check 'test3-3
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
		      (v1 (abortP p 7)))
		 (+ v1 10)))))
      (abortP p 9)
      (+ v 20)))
  'must-be-error)

;; (test-check 'test3-3-1
;;   (let ((p (new-prompt)))
;;     (let ((v (push-prompt p
;; 	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
;; 		      (v1 (abortP p 7)))
;; 		 (+ v1 10)))))
;;       (prompt-set? p)))
;;   false)

(test-check 'test4
  (let ((p (new-prompt)))
    (+ (push-prompt p 
	 (+ (take-subcont p sk (push-prompt p (push-subcont sk 5)))
	   10))
      20))
  35)

(the Array (array))
