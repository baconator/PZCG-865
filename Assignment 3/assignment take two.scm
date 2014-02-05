#lang racket
(define (solve input) (letrec
	(	[shortmap (lambda (proc lst) ;returns either an empty list or the first result that 'isn't' an empty list.
			(if (empty? lst) '() 
				(let ([result (proc (first lst))]) 
					(if (not (empty? result)) result 
						(shortmap proc (rest lst))))))]
		[colours (range 1 (+ 1 (second input)))]
		[vertices (make-immutable-hash (map (lambda (tuple) 
			(cons (first tuple) (rest tuple))) 
			(rest (rest input))))]
		[adjacent (curry hash-ref vertices)]
		[expand (lambda (assignment index) 
			(filter 
				(compose1 not (curry hash-ref assignment)) 
				(adjacent index)))]
		[lcv (lambda (assignment index) (let* ;applies the lcv heuristic. It occurs to me that I could simplify this with a curry operation... nah.
			(	[children (expand assignment index)]
				[size (lambda (colour) 
					(length (flatten (map
						(curry colourings (hash-set assignment index colour))
						children))))]) 
			(lambda (a b) 
				(< (size a) (size b)))))]
		[colourings (lambda (assignment index) ;gets the possible for a given node given an assignment
			(set->list (foldl (lambda (current set) 
					(set-remove set current)) 
				(apply set colours) 
				(filter-map 
					(curry hash-ref assignment) 
					(adjacent index)))))]
		[degree (lambda (assignment index) 
			(length (expand assignment index)))]
		[mrv (lambda (assignment) ;applies the mrv heuristic, then the degree heuristic if that fails.
			(lambda (a b) (let 
				(	[resA (length (colourings assignment a))]
					[resB (length (colourings assignment b))]) 
				(if (equal? resA resB)
					(> (degree assignment a) (degree assignment b))
					(< resA resB)))))]
		[getUnassigned (lambda (assignment) 
			(map car (filter (compose1 not cdr) (hash->list assignment))))]
		[search (lambda (assignment) (let ([unassigned (getUnassigned assignment)]) ;either return a valid assignment or an empty list (if no valid colourings)
			(if (empty? unassigned)
				assignment
				(let ([current (first (sort unassigned (mrv assignment)))]) 
					(shortmap (lambda (colour) 
						(search (hash-set assignment current colour)))
						(sort (colourings assignment current) 
							(lcv assignment current)))))))]
	)(flatten (search (make-immutable-hash (map (lambda (tuple) ;search returns a tree, flatten just makes that into a list.
		(cons (first tuple) #f)) (rest (rest input))))))
))