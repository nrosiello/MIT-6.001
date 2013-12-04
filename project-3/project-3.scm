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
