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

(use hahn)
(module graphs
  (
   <multidigraph>
   <multigraph>
   <digraph>
   <graph>
   make-multidigraph
   make-multigraph
   make-digraph
   make-graph
   graph-copy
   graph->list
   graph-attribute
   graph-attribute-set
   graph-attribute-set!
   digraph?
   multigraph?
   graph?
   graph-neighbours
   graph-vertex-exists?
   graph-adjacent?
   graph-vertex
   graph-vertices
   graph-vertex-add
   graph-vertex-add!
   graph-vertex-remove
   graph-vertex-remove!
   graph-vertex-update
   graph-vertex-update!
   graph-edge
   graph-edge-add
   graph-edge-add!
   graph-edge-remove
   graph-edge-remove!
   graph-edge-update
   graph-edge-update!
   graph-simple?
   graph-indegree
   graph-outdegree
   graph-degree
   graph-order
   )

  (import chicken scheme data-structures)
  (use arrays    ; Used to import sets
       coops
       coops-primitive-objects
       matchable
       srfi-1
       srfi-69)

  (import array-sets)

  (include "src/utils")
  (include "src/low-level")
  (include "src/classes")
  (include "src/multidigraph")
  (include "src/multigraph")
  (include "src/digraph")
  (include "src/graph")
  )
