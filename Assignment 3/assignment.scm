#lang racket

(define (firstMap proc lst) (let [(result (proc (first lst)))] 
	(cond 
		[(not (empty? result)) result]
		[(empty? lst) '()]
		[(firstMap proc (rest lst))]
	)
))

(define (solve input) (begin
	(define domain (range (second input)))
	(define vertices (list->vector (drop input 2)))
	
	(define (index->vertex index)
		(vector-ref vertices (- index 1))
	)
	
	(define (validValues vertex assignment) ;todo: implement lcv heuristic
		(set->list (foldl 
			(lambda (c a) (set-remove a c)) 
			(apply set domain) 
			(filter-map 
				(compose1 first (curry hash-ref assignment) index->vertex) 
				(rest vertex)
			)
		))
	)
	
	(define (degree vertex assignment) ;Number of unassigned children
		(length (filter-map 
			(compose1 not first (curry hash-ref assignment) index->vertex) 
			(rest vertex)
		))
	)
	
	(define (childValues vertex assignment)
		(foldl (lambda (child sum) 
			(+ (length (validValues child assignment)) sum)
		) 0 (expand vertex assignment '()))
	)
	
	(define (expand parent assignment oldFringe) 
		(append (sort (filter (lambda (vertex) ;cull visited vertices.
			(and (not (first (hash-ref assignment vertex))) (not (member vertex oldFringe))) ;unassinged/not in fringe. note: possibly collapse into degree
		)(map index->vertex (rest parent))) (lambda (a b)
			(< (length (validValues a assignment)) (length (validValues b assignment))) ;todo: implement degree heuristic.
		)) oldFringe)
	)
	
	(define (flood assignment fringe)
		(if (empty? fringe)
			assignment
			(let [(current (first fringe))]
				(firstMap (compose1 (lambda (nextAssignment) 
					(flood nextAssignment (expand current nextAssignment (rest fringe)))
				) (curry hash-set assignment current) list) (validValues current assignment)) ;need to '(value) to keep hash homogeneous.
			)
		)
	)
	
	(define (fill assignment)(let [(remaining (map caar (filter (compose1 not second) (hash->list assignment))))]
		(if (empty? remaining)
			assignment
			(flood assignment (expand (cons 0 remaining) assignment '()))
		)
	))
	(flatten (fill (make-immutable-hash (map (curryr list #f) (vector->list vertices)))))
))

(solve '(10 4
(1 2 3 4 6 7 10)
(2 1 3 4 5 6)
(3 1 2)
(4 1 2)
(5 2 6)
(6 1 2 5 7 8)
(7 1 6 8 9 10)
(8 6 7 9)
(9 7 8 10)
(10 1 7 9)
))