(begin
	(define 
		filter
		(
			lambda
			(li pred)
			(list-comp x li (* x 1) (eqv? (pred x) #t))
		)
	)
	(define
		par
		(
			lambda
			(x)
			(eqv? (mod x 2) 0)
		)
	)
	(filter (1 2 3 4 5 6 7 8) par)
)
