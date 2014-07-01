(begin
	(define 
		myListComp
		(
			lambda
			(li func pred)
			(if (eqv? li '())
				'()
				(let ((x (car li))
					 (r (func x))
					 (con (pred x)))
					
					 (if (eqv? con #t)
					 	 (cons r (myListComp (cdr li) func pred))
					 	 (myListComp (cdr li) func pred)
					 )
				)
			)
		)
	)
	(define
		square
		(
			lambda
			(x)
			(* x x)
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
	(myListComp (1 2 3 4 5 6 7 8) square par)
)
