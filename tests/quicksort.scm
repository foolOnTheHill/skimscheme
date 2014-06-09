(begin 
	(define filterByLess
		(
			lambda
			(lst piv)
			(if (eqv? lst '())
				'()
				(if (< (car lst) piv)
					(cons (car lst) (filterByLess (cdr lst) piv))
					(filterByLess (cdr lst) piv)
				)
			)
		)
	)
	(define filterByGreater
		(
			lambda
			(lst piv)
			(if (eqv? lst '())
				'()
				(if (>= (car lst) piv)
					(cons (car lst) (filterByGreater (cdr lst) piv))
					(filterByGreater (cdr lst) piv)
				)
			)
		)
	)
	(define quicksort
		(
			lambda
			(lst)
			(if (eqv? lst '())
				'()
				(append 
					(append 
						(quicksort 
							(filterByLess (cdr lst) (car lst))
						)
						(cons (car lst) '())
					)
					(quicksort
						(filterByGreater (cdr lst) (car lst))
					)
				)
			)
		)
	)
	(quicksort (1 2 3 7 8 9 0 11 -1 -4))
)