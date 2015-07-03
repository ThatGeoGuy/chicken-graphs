;;;; Copyright (C) 2015 Jeremy Steward
;;;;
;;;; This file is part of chicken-graphs
;;;;
;;;; chicken-graphs is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(use hahn)
(module graphs
  (
   <multidigraph>
   <multigraph>
   <digraph>
   <graph>
   print-object
   make-multidigraph
   make-multigraph
   make-digraph
   make-graph
   graph-copy
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
   )

  (import chicken scheme data-structures)
  (use arrays    ; Used to import sets
       coops
       coops-primitive-objects
       matchable
       srfi-1
       srfi-69)
  (import sets)

  (include "src/utils")
  (include "src/classes")
  (include "src/multidigraph")
  (include "src/multigraph")
  (include "src/digraph")
  (include "src/graph")
  )
