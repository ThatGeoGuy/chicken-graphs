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

(test-begin "Isomorphism")

(test-group "Isomorphic Identity"
  (test-generative ([vs (gen-list-of (gen-vertex-obj) (range 0 6))])
    (let ([G (make-graph)]
          [DG (make-digraph)]
          [edges (if (not (null? vs))
                   (zip vs (cdr vs))
                   '())])
      (for-each (lambda (graph-obj)
                  (for-each (lambda (vertex-pair)
                              (graph-edge-add! graph-obj
                                               (car vertex-pair)
                                               (cadr vertex-pair)))
                            edges))
                (list G DG))
      (test-assert "Graph is isomorphic with itself"
        (graph-isomorphic? G G))
      (test-assert "DiGraph is isomorphic with itself"
        (graph-isomorphic? DG DG)))

    (test-generative ([vs (gen-list-of (gen-vertex-obj) (range 0 6))])
      (let ([MG (make-multigraph)]
            [MDG (make-multidigraph)]
            [edges (if (not (null? '()))
                     (zip vs (cdr vs))
                     '())])
        (for-each (lambda (graph-obj)
                    (for-each (lambda (vertex-triplet)
                                (graph-edge-add! graph-obj
                                                 (car vertex-triplet)
                                                 (cadr vertex-triplet)
                                                 1))
                              edges))
                  (list MG MDG))
        (test-assert "MultiGraph is isomorphic with itself"
          (graph-isomorphic? MG MG))
        (test-assert "MultiDiGraph is isomorphic with itself"
          (graph-isomorphic? MDG MDG))))))

(test-end "Isomorphism")
