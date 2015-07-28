;;;; Copyright (c) 2015, Jeremy Steward
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;; 1. Redistributions of source code must retain the above copyright notice,
;;;; this list of conditions and the following disclaimer.
;;;;
;;;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;;; this list of conditions and the following disclaimer in the documentation
;;;; and/or other materials provided with the distribution.
;;;;
;;;; 3. Neither the name of the copyright holder nor the names of its
;;;; contributors may be used to endorse or promote products derived from this
;;;; software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES  INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(define (candidate-pairs G1 G2 s)
  (define (gen-candidates T1 m)
    (lazy-seq
      (if (null? T1)
        '()
        (cons (cons (car T1) m)
              (gen-candidates (cdr T1) m)))))
  (let ([T1 (set->list
              (set-difference (graph-vertices G1)
                              (set-map car s)))]
        [T2 (set->list
              (set-difference (graph-vertices G2)
                              (set-map cdr s)))])
    (if (not (null? T2))
      (gen-candidates T1 (car T2))
      (lazy-seq '()))))

(define (feas-rule-self G1 G2 s n m)
  @("Tests whether or not there are the same number of self loops in G1 and G2 at vertices n and m."
    "If these equal between the two graphs, then this procedure returns #t, otherwise #f."
    (G1 "First graph")
    (G2 "Second graph")
    (s "A set of partial matches")
    (n "The candidate from graph G1 to add to s")
    (m "The candidate from graph G2 to add to s")
    (gteq? "A greater than or equals comparator. This is to differentiate subgraph from graph isomorphism.")
    (@to "bool")
    (@no-source)
    (@internal))
  (= (set-count (set-filter (lambda (x)
                              (equal? (if (multigraph? G1)
                                        (car x)
                                        x)
                                      n))
                            (graph-neighbours G1 n)))
     (set-count (set-filter (lambda (x)
                              (equal? (if (multigraph? G2)
                                        (car x)
                                        x)
                                      m))
                            (graph-neighbours G2 m)))))

(define (feas-rule-neighbours G1 G2 s n m)
  @("Feasibility rule which evaluates the neighbours of n and m which are in our partial mapping."
    "Such neighbours (n' and m') should have the same number of edges n->n' as m->m' and for each"
    "neighbour n' of n there should exist a corresponding neighbour m' of m that matches."
    "NOTE: This should hold true both ways, so neighbours of G1 and G2 are both tested."
    (G1 "First graph")
    (G2 "Second graph")
    (s "A set of partial matches")
    (n "The candidate from graph G1 to add to s")
    (m "The candidate from graph G2 to add to s")
    (gteq? "A greater than or equals comparator. This is to differentiate subgraph from graph isomorphism.")
    (@to "bool")
    (@no-source)
    (@internal))
  ;; Defined as a function to avoid repetition where possible
  (define (candidate-neighbours-match? G1 G2 s n m)
    (call/cc
      (lambda (k)
        (set-for-each (lambda (neighbour)
                        (let* ([match (set-find (lambda (x)
                                                  (equal? (car x)
                                                          neighbour))
                                                s)]
                               [m-prime (if match (cdr match) #f)])
                          (when m-prime
                            (unless (graph-adjacent? G2 m m-prime)
                              (k #f))
                            (unless (= (num-adjacencies G1 neighbour n)
                                       (num-adjacencies G2 m-prime m))
                              (k #f)))))
                      (graph-neighbours G1 n))
        #t)))
  (and (candidate-neighbours-match? G1 G2 s n m)
       ;; The set has to be flipped below to account for n and m being flipped
       (candidate-neighbours-match? G2 G1
                                    (set-map (lambda (x) (cons (cdr x) (car x))) s)
                                    m n)))

(define (feas-rule-inout G1 G2 s n m gteq?)
  @("Feasibility rule which evaluates the number of neighbours of our partial matches."
    "The `in` rule specifies that the number of edges going into each of the candidate nodes must be equal."
    "The `out` rule specifies that the number of edges going out of each candidate node must be equal."
    "In both the `in` and `out` rules, we only consider the number of neighbours not already in our partial mapping."
    (G1 "First graph")
    (G2 "Second graph")
    (s "A set of partial matches")
    (n "The candidate from graph G1 to add to s")
    (m "The candidate from graph G2 to add to s")
    (gteq? "A greater than or equals comparator. This is to differentiate subgraph from graph isomorphism.")
    (@to "bool")
    (@no-source)
    (@internal))
  (and (gteq? (graph-outdegree G1 n weighted: #f)
              (graph-outdegree G2 m weighted: #f))
       (gteq? (graph-indegree G1 n weighted: #f)
              (graph-indegree G2 m weighted: #f))))

(define (syntactic-feasibility? G1 G2 s n m gteq?)
  @("Predicate which tests for syntactic feasibility."
    "This is based off of the five rules listed in the original VF2 paper, however, there are some differences."
    "Most notably, there are only three helper functions for five rules. This is because the original paper defined the steps specifically with regard to implementation details of the reference implementation in a very explicit fashion. The reason this implementation is shorter is because unlike the original paper, I have access to a `set` data structure, which simplifies some of the operations (e.g. no need for Pred or Succ, as sets are unordered)."
    (G1 "First graph")
    (G2 "Second graph")
    (s "A set of partial matches")
    (n "The candidate from graph G1 to add to s")
    (m "The candidate from graph G2 to add to s")
    (gteq? "A greater than or equals comparator. This is to differentiate subgraph from graph isomorphism.")
    (@to "bool")
    (@no-source)
    (@internal))
  (and (feas-rule-self G1 G2 s n m)
       (feas-rule-neighbours G1 G2 s n m)
       (feas-rule-inout G1 G2 s n m gteq?)))

(define (graph-match G1 G2 gteq? semantic-feasibility?)
  @("A procedure which performs the partial recursive matching between graphs G1 and G2."
    "Returns a lazy-seq which contains a list of the set of isomorphisms between G1 and G2."
    "Note that if subgraph matching is desired, gteq? must be >= and G1 is the graph that will be tested for subgraph isomorphism (i.e. only subgraphs in G1 are searched)."
    (G1 "First graph")
    (G2 "Second graph")
    (gteq? "A greater than or equals comparator. This is to differentiate subgraph from graph isomorphism.")
    (semantic-feasibility? "A predicate procedure for testing semantic feasibility between two graphs."
                           "See documentation in graph-isomorphic? for more details.")
    (@to "lazy-seq")
    (@no-source))
  (define (candidate-loop acc s c-pairs)
    (lazy-seq
      (cond
        [(set= (set-map cdr s)
               (graph-vertices G2))
         (cons s acc)]
        [(lazy-null? c-pairs) acc]
        [else
         (let ([n (car (lazy-head c-pairs))]
               [m (cdr (lazy-head c-pairs))])
           (if (and (syntactic-feasibility? G1 G2 s n m gteq?)
                    (semantic-feasibility? G1 G2 s n m))
             (let ([next-set (set-union s (set (cons n m)))])
               (candidate-loop (candidate-loop acc
                                               s
                                               (lazy-tail c-pairs))
                               next-set
                               (candidate-pairs G1 G2 next-set)))
             (candidate-loop acc s (lazy-tail c-pairs))))])))
  (candidate-loop (lazy-seq '()) (make-set) (candidate-pairs G1 G2 (make-set))))

(define (graph-isomorphisms G1 G2 #!optional (semantic-feasibility?
                                                     (lambda (G1 G2 s n m) #t)))
  @("Returns a lazy sequence of all the isomorphisms from G1 to G2. If none exist, the sequence will be null."
    (G1 "First graph")
    (G2 "Second graph")
    (semantic-feasibility? "A predicate procedure for testing semantic feasibility between two graphs."
                           "See argument information in `graph-isomorphic?` for more details.")
    (@to "lazy-seq")
    (@no-source))
  (graph-match G1 G2 = semantic-feasibility?))

(define (graph-isomorphisms-list G1 G2 #!optional (semantic-feasibility?
                                                  (lambda (G1 G2 s n m) #t)))
  (lazy-seq->list (graph-isomorphisms G1 G2 semantic-feasibility?)))

(define (subgraph-isomorphisms G1 G2 #!optional (semantic-feasibility?
                                                     (lambda (G1 G2 s n m) #t)))
  @("Returns a lazy sequence of all the isomorphisms from G1 to G2. If none exist, the sequence will be null."
    (G1 "First graph")
    (G2 "Second graph")
    (semantic-feasibility? "A predicate procedure for testing semantic feasibility between two graphs."
                           "See argument information in `graph-isomorphic?` for more details.")
    (@to "lazy-seq")
    (@no-source))
  (graph-match G1 G2 >= semantic-feasibility?))

(define (subgraph-isomorphisms-list G1 G2 #!optional (semantic-feasibility?
                                                      (lambda (G1 G2 s n m) #t)))
  (lazy-seq->list (subgraph-isomorphisms G1 G2 semantic-feasibility?)))

(define (graph-isomorphic? G1 G2 #!optional (semantic-feasibility?
                                              (lambda (G1 G2 s n m) #t)))
  @("Tests whether two graphs are isomorphic, using the VF2 algorithm."
    "See: http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1323804"
    "If semantic-feasibility? is provided, then a semantic check is likewise performed."
    (G1 "First graph")
    (G2 "Second graph")
    (semantic-feasibility? "A predicate procedure for testing semantic feasibility between two graphs."
                           "Should be a procedure taking arguments (G1 G2 s n m), where G1 and G2 are the two graphs, s is a set of cons pairs describing the partial matching between G1 and G2, and n and m are candidate vertices in G1 and G2 respectively."
                           "The procedure should evaluate to #t iff the attributes of the vertices (or their edges) are semantically feasible. The default procedure for testing this always evaluates to #t, which effectively means attribute information is ignored.")
    (@to "bool")
    (@no-source))
  (cond
    [(not (eqv? (graph-order G1)
                (graph-order G2)))
     #f]
    [else (not (lazy-null? (graph-isomorphisms G1 G2 semantic-feasibility?)))]))

(define (subgraph-isomorphic? G1 G2 #!optional (semantic-feasibility?
                                                 (lambda (G1 G2 s n m) #t)))
  @("Performs the same tests as graph-isomorphic?, however tests for any subgraph isomorphism in G1 to G2."
    "The only difference is that in graph-match, >= is passed instead of =."
    "See graph-isomorphic? for more information on arguments."
    (G1 "First graph")
    (G2 "Second graph")
    (semantic-feasibility? "A predicate procedure for testing semantic feasibility between two graphs.")
    (@to "bool")
    (@no-source))
  (not (lazy-null? (subgraph-isomorphisms G1 G2 semantic-feasibility?))))
