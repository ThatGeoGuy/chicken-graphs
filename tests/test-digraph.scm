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

(test-begin "Digraph Properties")
(test-group "identity properties"
  (let* ([v (gen-vertex-obj)]
         [G1 (make-digraph)]
         [G2 (graph-vertex-remove (graph-vertex-add G1 v) v)])
    (test-assert "Vertex identity - adjacency lists should be empty"
                 (and (equal? (graph->list G1) '())
                      (equal? (graph->list G2) '()))))

  (let* ([u (gen-vertex-obj)]
         [v (gen-vertex-obj)]
         [G1 (make-digraph)]
         [G2 (graph-edge-remove (graph-edge-add G1 u v) u v)])
    (test-assert "Edge identity - v should not be adjacent to u"
                 (not (graph-adjacent? G2 u v)))
    (test-assert "Edge identity - u should not be adjacent to v"
                 (not (graph-adjacent? G2 v u)))
    (test-assert "Edge identity - neighbours of u should be empty set"
                 (set-null? (graph-neighbours G2 u)))
    (test-assert "Edge identity - neighbours of v should be empty set"
                 (set-null? (graph-neighbours G2 v)))
    (test-assert "Edge identity - vertex u should still exist despite edge removal"
                 (graph-vertex-exists? G2 u))
    (test-assert "Edge identity - vertex v should still exist despite edge removal"
                 (graph-vertex-exists? G2 v))
    (test-assert "Edge identity - original graph should not contain u->v"
                 (not (graph-adjacent? G1 u v)))
    (test-assert "Edge identity - original graph should not have u or v vertices"
                 (not (or (graph-vertex-exists? G1 u)
                          (graph-vertex-exists? G1 v))))))

(test-group "commutativity properties"
  (let* ([u (gen-vertex-obj)]
         [v (let loop ([v (gen-vertex-obj)])
              (if (equal? u v) (loop (gen-vertex-obj)) v))])
    (let* ([G1 (make-digraph)]
           [G2 (graph-vertex-add (graph-vertex-add G1 u) v)]
           [G3 (graph-vertex-add (graph-vertex-add G1 v) u)]
           [G4 (graph-vertex-remove (graph-vertex-remove G2 u) v)]
           [G5 (graph-vertex-remove (graph-vertex-remove G3 v) u)])
      (test-assert "Vertex commutativity - u should be present in both graphs"
                   (and (graph-vertex-exists? G2 u)
                        (graph-vertex-exists? G3 u)))
      (test-assert "Vertex commutativity - v should be present in both graphs"
                   (and (graph-vertex-exists? G2 v)
                        (graph-vertex-exists? G3 v)))
      (test-assert "Vertex commutativity - order of removal should not matter"
                   (and (not (or (graph-vertex-exists? G4 u)
                                 (graph-vertex-exists? G4 v)))
                        (not (or (graph-vertex-exists? G5 u)
                                 (graph-vertex-exists? G5 v))))))

    (let* ([G1 (make-digraph)]
           [G2 (graph-edge-add G1 u v)]
           [G3 (graph-edge-add G1 v u)]
           [G4 (graph-edge-remove G2 u v)]
           [G5 (graph-edge-remove G3 v u)])
      (test-assert "Edge commutativity - u should be adjacent to v"
                   (graph-adjacent? G2 u v))
      (test-assert "Edge commutativity - v should be adjacent to u"
                   (graph-adjacent? G3 v u))
      (test-assert "Edge commutativity - removing edge u<->v: order should not matter"
                   ;; All tests are still valid since no edges should be present at all
                   (not (or (graph-adjacent? G4 u v)
                            (graph-adjacent? G4 v u)
                            (graph-adjacent? G5 u v)
                            (graph-adjacent? G5 v u)))))))

(test-end "Digraph Properties")
