(begin
	(define
		map
		(
			lambda
			(li f)
			(list-comp x li (f x) #t)
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
	(map (1 2 3 4 5 6 7) square)
)
