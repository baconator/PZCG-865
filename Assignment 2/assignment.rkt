#lang racket
(require data/heap)

(define (aStar dimensions start goal maze) (begin
	(define globalFringe (make-heap (lambda (a b) 
		(> (cost a) (cost b)) ;sorts the heap such that the top value is the largest, not smallest
	)))

	(define (aStarP fringe expand cost goalTest)
		(cond 
			[(equal? (heap-count fringe) 0) (list #f)]
			[(goalTest (heap-min fringe)) (heap-min fringe)]
			[#t (let ([current (heap-min fringe)])
				(begin 
					(heap-remove-min! fringe)
					(heap-add-all! fringe (expand current))
					(display (heap->vector fringe)) (newline)
					(aStarP fringe expand cost goalTest)
				)
			)]
		)
	)

	(define (expander currentChain) ;nodes are represented as a list where the head is the current location
		(filter-map (lambda (next) ;only returns paths that don't contain cycles
			(and (not (member next currentChain)) (append (list next) currentChain))
		)(rest (findf (lambda (line) ;finds equivalent line in maze
			(equal? (first currentChain) (first line))
		) maze)))
	)

	(define (diff x y) (abs (- x y)))

	(define (cost chain)
		(+ 
			(length chain) ;path cost, shouldn't have to worry about the off by one
			(+ (diff (first (first chain)) (first goal)) (diff (second (first chain)) (second goal))) ;manhattan distance
		)
	)

	(heap-add! globalFringe (list start))
	(reverse (aStarP globalFringe expander cost (lambda (chain) (equal? (first chain) goal))))
))

(define (hillClimbing dimensions start goal maze) (begin ;I really should just pass in the heuristic function as a lambda, but I am too lazy.
	(define (expand currentChain) ;nodes are represented as a list where the head is the current location
		(filter-map (lambda (next) ;only returns paths that don't contain cycles
			(and (not (member next currentChain)) (append (list next) currentChain))
		)(rest (findf (lambda (line) ;finds equivalent line in maze
			(equal? (first currentChain) (first line))
		) maze)))
	)
	
	(define (diff x y) (abs (- x y)))
	
	(define (cost chain) 
		(+ (diff (first (first chain)) (first goal)) (diff (second (first chain)) (second goal))) ;manhattan distance
	)
	
	(define (compare a b) (> (cost a) (cost b)))
	
	(define (climb chains goalTest)
		(cond 
			[(equal? (length chains) 0) (list #f)] 
			[(goalTest (first chains)) (first chains)]
			[#t (begin 
				(display (first chains)) (newline)
				(climb (append (sort (expand (first chains)) compare) (rest chains)) goalTest)
			)]
		)
	)
	(reverse (climb (list (list start)) (lambda (chain) (equal? (first chain) goal))))
))