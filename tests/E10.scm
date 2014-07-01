(begin
	(define
		length
		(
			lambda
			(li)
			(if (eqv? li '())
				0
				(+ 1 (length (cdr li)))
			)
		)
	)
	(define
		foldr
		(
			lambda
			(f li)
			(let ((s (length li))
				  (con (eqv? s 1))
				  (x (car li))
				  (xs (cdr li)))

				 (if (eqv? con #t)
				 	 x
				 	 (f x (foldr f xs))
				 )
			)
		)
	)

	(foldr (lambda (x y) (+ x y)) (1 2 3 4 5 6))
)
