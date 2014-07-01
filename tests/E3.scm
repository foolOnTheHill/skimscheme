(begin 
	(define-struct node '(value left right))
	(new node leaf1 (1 null null))
	(new node leaf2 (4 null null))
	(new node leaf3 (6 null null))
	(new node leaf4 (10 null null))
	(new node node1 (3 leaf1 leaf2))
	(new node node2 (7 leaf3 null))
	(new node node3 (8 node2 leaf4))
	(new node root (5 node1 node3))

	(define 
		inorder
		(lambda
			(n)
			(if (eqv? n null)
				'()
				(append 
					(append 
						(inorder (getAttribute n left)) (cons (getAttribute n value) '())
					)
					(inorder (getAttribute n right))
				)
			)
		)
	)

	(inorder root)
)