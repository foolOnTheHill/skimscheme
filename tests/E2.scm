(begin 
	(define quicksort
		(
			lambda
			(lst)
			(if (eqv? lst '())
				'()
				(append 
					(append 
						(quicksort 
							(list-comp x (cdr lst) (* x 1) (< x (car lst)))
						)
						(cons (car lst) '())
					)
					(quicksort
						(list-comp y (cdr lst) (* y 1) (>= y (car lst)))
					)
				)
			)
		)
	)
	(quicksort (1 5 6 7 2 3 725 23 178))
)