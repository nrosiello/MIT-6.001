;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; warmup exercise 1: add to index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; code in search.scm for add-to-index function modified

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; warmup exercise 2: web as a general graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the initial implementation of DFS-simple will fail on the-web graph
;; because the graph contains cycles.  for example, the SchemeImplementations
;; and getting-help pages both link to each other.  since the search
;; implementation currently does not check which pages have already been
;; visited, it will search infinitely through the cycles.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 1: breadth-first search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first ensure that the current DFS implementation is working
;; before modifying it
(write-line "Testing DFS-simple")
(DFS-simple 'a
            (lambda (node) (eq? node '1))
            test-graph)

;; implement breadth-first search through a simple modification of DFS
(write-line "Testing BFS-simple")
(define (BFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append old new))
          graph))

(BFS-simple 'a
            (lambda (node) (eq? node '1))
            test-graph)

;; this works because instead of putting the children nodes at the front
;; of the line to be explored, they are appended to the back, meaning that
;; each level is joined together as it is discovered

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 2: marking visited nodes in graph search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-with-cycles initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (new-node? node)
    (let ((new? 
            (null? (find-in-index nodes-searched node))))
      (if new?
          (add-to-index! nodes-searched node '()))
      new?))
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
    (if (new-node? current) 
	    (if (goal? current)
	       #t
	       (search-inner
	        (merge (successors graph current) (cdr still-to-do))))
      (search-inner (cdr still-to-do))))))
  (define nodes-searched (make-index))
  (search-inner (list initial-state)))

;; revised DFS that works with cycles
(define (DFS start goal? graph)
  (search-with-cycles start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))

;; test new DFS
(write-line "Testing DFS which allows cycles")
(DFS 'a
     (lambda (node) (eq? node '1))
     test-cycle)

;; revised BFS that works with cycles
(define (BFS start goal? graph)
  (search-with-cycles start
     goal?
     find-node-children
     (lambda (new old) (append old new))
     graph))

;; test new BFS
(write-line "Testing BFS which allows cycles")
(BFS 'a
     (lambda (node) (eq? node '1))
     test-cycle)

;; test on the-web graph
(write-line "Testing DFS on the web")
(DFS 'http://sicp.csail.mit.edu/
     (lambda (node) #f)
     the-web)

(write-line "Testing BFS on the web")
(BFS 'http://sicp.csail.mit.edu/
     (lambda (node) #f)
     the-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 3: index abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; same as warm-up exercise 1, code and tests are in search.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 4: a web index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-document-to-index!: Index, Web, URL
(define (add-document-to-index! index web url)
  (define (add-documents-inner! words)
    (if (null? words)
      '()
      (let ((new-word (car words))
            (remaining-words (cdr words)))
        (add-to-index! index new-word url)
        (add-documents-inner! remaining-words))))
  (add-documents-inner! (find-URL-text web url)) 
)

;; test cases for add-document-to-index
(define the-web-index (make-index))
(add-document-to-index! the-web-index
                        the-web
                        'http://sicp.csail.mit.edu/)
(test-equal (find-in-index the-web-index 'help)
            '(http://sicp.csail.mit.edu/))
(test-equal (find-in-index the-web-index '*magic*)
            '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 5: crawling the web to build an index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create a new set of search procedures that allows a procedure
;; to be run at each node
(define (search-with-action initial-state goal? successors merge graph action)
  (define (new-node? node)
    (let ((new? 
            (null? (find-in-index nodes-searched node))))
      (if new?
          (add-to-index! nodes-searched node '()))
      new?))
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
    (if (new-node? current)
      (begin
        (action current)
	      (if (goal? current)
	        #t
	        (search-inner
	         (merge (successors graph current) (cdr still-to-do)))))
      (search-inner (cdr still-to-do))))))
  (define nodes-searched (make-index))
  (search-inner (list initial-state)))

;; revised DFS with actions 
(define (DFS-action start goal? graph action)
  (search-with-action start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph
    action))

;; revised BFS with actions 
(define (BFS-action start goal? graph action)
  (search-with-action start
     goal?
     find-node-children
     (lambda (new old) (append old new))
     graph
     action))

;; test BFS with actions
(write-line "Testing BFS with actions on the web graph")
(BFS-action 'http://sicp.csail.mit.edu/
     (lambda (node) #f)
     the-web
     (lambda (node) (write-line (list 'action-at node))))

;; given a web graph, create an index from words -> URLs
(define (make-web-index web start-url)
  (define web-index (make-index))
  (BFS-action start-url
     (lambda (node) #f)
     web
     (lambda (node) (add-document-to-index! web-index
                                            web
                                            node)))
  (lambda (word)
    (find-in-index web-index word)))
   
;; web index test cases
(define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))
(test-equal (find-documents 'collaborative)
            '(http://sicp.csail.mit.edu/ http://sicp.csail.mit.edu/psets))
(test-equal (find-documents '*fake-word*)
            '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 6: dynamic web search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extracted web crawling function to be used for search-any and search-all
(define (search-web web start-node word stop?)
  (define results '())
  (define stop-node? (stop? results))
  (define append-result-at-node
    (lambda (node)
      (define (append-result)
        (if (null? results)
          (set! results (list node))
          (append! results (list node))))
      (if (memq word (find-URL-text the-web node))
        (append-result))))

  (BFS-action start-node 
     stop-node? 
     web
     append-result-at-node)
  results)

;; search-any crawls the web breadth-first looking for the first document
;; containing the desired word
(define (search-any web start-node word)
  (search-web web
     start-node
     word
     (lambda (results)
       (lambda (node)
         (not (null? results))))))

;; search-all crawls the web looking for all documents that contain the
;; desired word
(define (search-all web start-node word)
  (search-web web
     start-node
     word
     (lambda (results)
       (lambda (node) #f))))

;; crawling functions test cases
(test-equal (search-any the-web 'http://sicp.csail.mit.edu/ 'collaborative)
            '(http://sicp.csail.mit.edu/))
(test-equal (search-any the-web 'http://sicp.csail.mit.edu/ '*fake-word*)
            '())
(test-equal (search-all the-web 'http://sicp.csail.mit.edu/ 'collaborative)
            '(http://sicp.csail.mit.edu/ http://sicp.csail.mit.edu/psets))
(test-equal (search-all the-web 'http://sicp.csail.mit.edu/ '*fake-word*)
            '())
