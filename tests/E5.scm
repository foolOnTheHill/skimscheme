(begin

	(define-struct player '(name hp mana level exp remainingExp))

	(define
		getRemainingExp
		(
			lambda
			'()
			(- (* 100 (+ this:level 1)) this:exp)
		)
	)

	(define
		attack
		(
			lambda
			(hero opponent)
			(let (
					(eHP (getAttribute opponent hp))
					(myHP (getAttribute hero hp))
					(myMana (getAttribute hero mana))
					(myXP (getAttribute hero exp))
				 )

				(if (lt? myHP eHP)
					#f
					(begin 
						(setAttribute opponent hp (- eHP myMana))
						(setAttribute hero hp (- myHP eHP))
						(setAttribute hero exp (+ myXP 10))
						#t
					)
				)
			)
		)
	)

	(new player cloud ("Cloud Strife" 500 100 10 50 getRemainingExp))
	(new player aeris ("Aerith Gainsborough" 300 600 11 500 getRemainingExp))

	(attack cloud aeris)
	(applyMethod cloud remainingExp '())
)