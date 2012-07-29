(assert (eq #t #t))
(assert (not (eq #t #f)))
(assert (eq #t (call/cc (lambda (k) (k #t) #f))))
(assert (eq #f (call/cc (lambda #ign #f))))
