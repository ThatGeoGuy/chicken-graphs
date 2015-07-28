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

(test-begin "Primitive procedures")

(test-group "Testing make procedures for constructing new graphs"
  (test-assert "make-graph returns an object of type graph"
    (eq? (class-of (make-graph)) <graph>))
  (test-assert "make-digraph returns an object of type digraph"
    (eq? (class-of (make-digraph)) <digraph>))
  (test-assert "make-multigraph returns an object of type multigraph"
    (eq? (class-of (make-multigraph)) <multigraph>))
  (test-assert "make-multidigraph returns an object of type multidigraph"
    (eq? (class-of (make-multidigraph)) <multidigraph>)))

(test-group "Testing predicates"
  (test-group "graph? predicate should be #t for all graph types"
    (test-assert "Graphs are graphs"
      (graph? (make-graph)))
    (test-assert "DiGraphs are graphs"
      (graph? (make-digraph)))
    (test-assert "MultiGraphs are graphs"
      (graph? (make-multigraph)))
    (test-assert "MultiDiGraphs are graphs"
      (graph? (make-multidigraph))))

  (test-group "digraph? predicate should be #t only for digraph types"
    (test-assert "Graphs are not digraphs"
      (not (digraph? (make-graph))))
    (test-assert "DiGraphs are digraphs"
      (digraph? (make-digraph)))
    (test-assert "MultiGraphs are not digraphs"
      (not (digraph? (make-multigraph))))
    (test-assert "MultiDiGraphs are digraphs"
      (digraph? (make-multidigraph))))

  (test-group "multigraph? predicate should be #t only for multigraph types"
    (test-assert "Graphs are not multigraphs"
      (not (multigraph? (make-graph))))
    (test-assert "DiGraphs are not multigraphs"
      (not (multigraph? (make-digraph))))
    (test-assert "MultiGraphs are multigraphs"
      (multigraph? (make-multigraph)))
    (test-assert "MultiDiGraphs are multigraphs"
      (multigraph? (make-multidigraph))))
  )

(test-end "Primitive procedures")
