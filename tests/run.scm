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

(use arrays
     coops
     data-generators
     posix
     test
     test-generative)

(import array-sets)

;; Every test needs to load the module first
;; The module should be useable without having to install it first
(use graphs
     graphs-derived)

(define (xor #!rest args)
  (call/cc
    (lambda (k)
      (foldl (lambda (knil x)
               (if (not (and knil x))
                 (or knil x)
                 (k #f)))
             #f
             args))))

(include "test-primitives.scm")

(define (gen-vertex-obj)
  (gen-sample-of
    (lambda ()
      (string->symbol
        ((gen-string-of (gen-char #\0 #\z)
                        (lambda () 8)))))
    (gen-string-of (gen-char #\0 #\z)
                   (lambda () 8))
    (gen-fixnum)))

(include "test-digraph.scm")
(include "test-graph.scm")
(include "test-multidigraph.scm")
(include "test-multigraph.scm")

;; Generates empty graphs => assumes make functions work by default
(define (gen-empty-graph-type)
  (gen-sample-of make-graph
                 make-digraph
                 make-multigraph
                 make-multidigraph))

;; Generates graph with random vertices / edges
;; Assumes graph-edge-add! works, which is why this is _after_
;; including test-(multi)(di)graph.scm
(define (gen-graph #!optional (order (range 0 10)))
  (lambda ()
    (let ([G ((gen-empty-graph-type))]
          [order ((gen-fixnum (if (pair? order)
                                order
                                (range 0 order))))])
      (let* ([vs ((gen-list-of (gen-vertex-obj) order))]
             [ids ((gen-list-of (gen-fixnum
                                  (range 0 (length vs)))))])
        (for-each
          (lambda (v-id-pair)
            (let ([v (car v-id-pair)]
                  [id (cadr v-id-pair)])
              (for-each (lambda (u)
                          (cond
                            [(and (multigraph? G)
                                  (not (graph-adjacent? G u v id)))
                             (graph-edge-add! G u v id)]
                            [(not (graph-adjacent? G u v))
                             (graph-edge-add! G u v)]))
                        vs)))
          (zip vs ids))
        G))))

(include "test-isomorphism.scm")
(test-exit)
