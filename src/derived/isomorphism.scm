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

(define (candidate-pairs G1 G2)
  ;; TODO Logic for producing candidate pairs
  )

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
  (foldl (lambda (k f)
           (and k (f s n m)))
         #t
         (list feasibility-rule-pred
               feasibility-rule-succ
               feasibility-rule-in
               feasibility-rule-out
               feasibility-rule-new)))

(define (semantic-feasibility? s n m)
  ;; TODO Implement a more appropriate semantic feasibility later
  #t)

(define (graph-match G1 G2)
  (let ([matchings (make-set)])
   (let match-loop ([s (make-set)])
    (cond
      [(set= (set-map cdr s)
             (graph-vertices G2))
       (set-add! s matchings)]
      [else (for-each (lambda (vertex-pair)
                        (let ([n (car vertex-pair)]
                              [m (cdr vertex-pair)])
                          (cond
                            [(and (syntactic-feasibility? s n m)
                                  (semantic-feasibility? s n m))
                             (match-loop (set-union s (cons n m))
                                         (cdr P))]
                            [else (match-loop s (cdr P))])))
                      (candidate-pairs s G1 G2))]))
   (set->list matchings)))

(define (isomorphic? G1 G2 #!key (semantic #f))
  @("Tests whether two graphs are isomorphic, using the VF2 algorithm."
    "See: http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1323804"
    "If semantic is provided and non-false, then a semantic check is likewise performed."
    (@to "set")
    (@no-source))
  (unless (eq? (graph-order G1)
               (graph-order G2))
    #f)
  ;; TODO Logic for finding a mapping
  )
