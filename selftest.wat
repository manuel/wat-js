(assert (wat-num= ((lambda (x) x) 3) 3))
(assert (wat-num= ((lambda (x) x) (wat-+ 3 4)) 7))
