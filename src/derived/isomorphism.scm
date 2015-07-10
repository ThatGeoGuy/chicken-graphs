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

(define (lazy-flatten seq-tree)
  (cond
    [(lazy-null? seq-tree) (lazy-seq '())]
    [(lazy-seq? (lazy-head seq-tree))
     (lazy-append (lazy-flatten (lazy-head seq-tree))
                  (lazy-flatten (lazy-tail seq-tree)))]
    [else (lazy-seq (cons (lazy-head seq-tree)
                          (lazy-flatten (lazy-tail seq-tree))))]))

(define (pred G vs)
  ;; TODO Implement procedure to return predecessors of vs
  )

(define (succ G vs)
  ;; TODO Implement procedure to return successors of vs
  )

(define (candidate-pairs s G1 G2)
  (let ([T1 (set->list
              (set-difference (graph-vertices G1)
                              (set-map car s)))]
        [T2 (set->list
              (set-difference (graph-vertices G2)
                              (set-map cdr s)))])
    (if (not (null? T1))
      (map (lambda (y)
             (cons (car T1) y))
           T2)
      '())))

(define (feas-rule-self G1 G2 s n m)
  @("Tests whether or not there are the same number of self loops in G1 and G2 at vertices n and m."
    "If these equal between the two graphs, then this procedure returns #t, otherwise #f."
    (@to "bool")
    (@no-source))
  (= (set-count (set-filter (lambda (x)
                              (equal? (if (multigraph? G1)
                                        (car x)
                                        x)
                                      n))
                            (graph-neighbours G1)))
     (set-count (set-filter (lambda (x)
                              (equal? (if (multigraph? G2)
                                        (car x)
                                        x)
                                      m))
                            (graph-neighbours G2)))))

(define (num-adjacencies G u v)
    @("Calculates the number of adjacencies (edges) between vertices u and v"
      (@to "integer")
      (@no-source))
    (set-count (set-filter (lambda (x)
                             (equal? (if (multigraph? G) (car x) x)
                                     v))
                           (graph-neighbours G u))))

(define (feas-rule-neighbours G1 G2 s n m)
  @("Feasibility rule which evaluates the neighbours of n and m which are in our partial mapping."
    "Such neighbours (n' and m') should have the same number of edges n->n' as m->m' and for each"
    "neighbour n' of n there should exist a corresponding neighbour m' of m that matches."
    (@to "bool")
    (@no-source))
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

(define (syntactic-feasibility? s n m)
  #t)

(define (semantic-feasibility? s n m)
  ;; TODO Implement a more appropriate semantic feasibility later
  #t)

(define (graph-match G1 G2)
  (let match-loop ([s (make-set)])
   (cond
     [(set= (set-map cdr s)
            (graph-vertices G2))
      s]
     [else (lazy-flatten
             (foldl (lambda (matchings vertex-pair)
                      (let ([n (car vertex-pair)]
                            [m (cdr vertex-pair)])
                        (if (and (syntactic-feasibility? s n m)
                                 (semantic-feasibility? s n m))
                          (lazy-seq (cons (match-loop (set-union s (set (cons n m))))
                                          matchings))
                          matchings)))
                    (make-lazy-seq (lambda () '()))
                    (candidate-pairs s G1 G2)))])))

(define (isomorphic? G1 G2 #!key (semantic #f))
  @("Tests whether two graphs are isomorphic, using the VF2 algorithm."
    "See: http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1323804"
    "If semantic is provided and non-false, then a semantic check is likewise performed."
    (@to "set")
    (@no-source))
  (unless (eq? (graph-order G1)
               (graph-order G2))
    #f)
  (unless (equal? (map (cute graph-degree G1 <>)
                       (set->list (graph-vertices G1)))
                  (map (cute graph-degree G2 <>)
                       (set->list (graph-vertices G2))))
    #f)
  ;; TODO Logic for finding a mapping
  )
