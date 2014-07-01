(begin
	(define a 1024)
	(define b 64)
	(define c 0)
	(define d #t)
	
	(let 
		((tmp (/ a b)))
		(if (eqv? tmp 0)
			(set! d #f)
			(set! c tmp)
		)
	)
	
	c
)
