#lang racket

(define (interleave left right) 
	(cond 
		((and (pair? left) (pair? right)) 
			(append (list (car left) (car right)) (interleave (cdr left) (cdr right)))
		)
		((pair? left)
			left
		)
		((pair? right) 
			right
		)
		(#t
			'()
		)
	)
)

(define (mapPrime func input)
	(if (null? input)
		'()
		(cons (func (car input)) (map func (cdr input)))
	)
)

(define (sub target replacer input)
	(if (pair? input)
		(mapPrime (lambda (inputPrime) 
			(sub target replacer inputPrime)
		) input)
		(if (equal? input target)
			replacer
			input
		)
	)
)