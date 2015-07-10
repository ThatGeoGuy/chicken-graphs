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

(define (pred G vs)
  ;; TODO Implement procedure to return predecessors of vs
  )

(define (succ G vs)
  ;; TODO Implement procedure to return successors of vs
  )

(define (candidate-pairs s G1 G2)
  ;; TODO Logic for producing candidate pairs
  (let ([T1 (set->list
              (set-difference (graph-vertices G1)
                              (set-map car s)))]
        [T2 (set->list
              (set-difference (graph-vertices G2)
                              (set-map cdr s)))])
    (concatenate (map (lambda (x)
                        (map (lambda (y)
                               (cons x y))
                             T2))
                      T1))))

(define (feasibility-rule-pred s n m)
  ;; TODO Implement eqn (3) from VF2 paper
  )

(define (feasibility-rule-succ s n m)
  ;; TODO Implement eqn (4) from VF2 paper
  )

(define (feasibility-rule-in s n m)
  ;; TODO Implement eqn (5) from VF2 paper
  )

(define (feasibility-rule-out s n m)
  ;; TODO Implement eqn (6) from VF2 paper
  )

(define (feasibility-rule-new s n m)
  ;; TODO Implement eqn (7) from VF2 paper
  )

(define (syntactic-feasibility? s n m)
  ;(call/cc
  ;  (lambda (k)
  ;    (for-each (lambda (f)
  ;                (unless (f s n m)
  ;                  (k #f)))
  ;              (list feasibility-rule-pred
  ;                    feasibility-rule-succ
  ;                    feasibility-rule-in
  ;                    feasibility-rule-out
  ;                    feasibility-rule-new))
  ;    #t))
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
     [else (foldl (lambda (matchings vertex-pair)
                    (let ([n (car vertex-pair)]
                          [m (cdr vertex-pair)])
                      (if (and (syntactic-feasibility? s n m)
                               (semantic-feasibility? s n m))
                        (append matchings
                                (flatten
                                  (list
                                    (match-loop (set-union s (set (cons n m))))))))))
                  '()
                  (candidate-pairs s G1 G2))])))

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
