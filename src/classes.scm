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

(define-class <abstract-graph> ()
  ([gattr  initform: (make-hash-table test: equal?) reader: graph-attr]
   [vattr initform: (make-hash-table test: equal?) reader: graph-vertex-attr]
   [atbl  initform: (make-hash-table test: equal?) reader: adjacency-table]))

(define-class <multidigraph> (<abstract-graph>)
  ())

(define-class <multigraph> (<multidigraph>)
  ())

(define-class <digraph> (<abstract-graph>)
  ())

(define-class <graph> (<digraph>)
  ())

(define-generic (graph-copy g))
(define-generic (graph->alist g))
(define-generic (graph->adj-list g))
(define-generic (graph-attribute g))
(define-generic (graph-attribute-set g))
(define-generic (graph-attribute-set! g))
(define-generic (graph-vertex-exists? g))
(define-generic (graph-adjacent? g))
(define-generic (graph-neighbours g))
(define-generic (graph-vertex g))
(define-generic (graph-vertex-add g))
(define-generic (graph-vertex-add! g))
(define-generic (graph-vertex-remove g))
(define-generic (graph-vertex-remove! g))
(define-generic (graph-vertex-update g))
(define-generic (graph-vertex-update! g))
(define-generic (graph-edge g))
(define-generic (graph-edge-add g))
(define-generic (graph-edge-add! g))
(define-generic (graph-edge-remove g))
(define-generic (graph-edge-remove! g))
(define-generic (graph-edge-update g))
(define-generic (graph-edge-update! g))
(define-generic (graph-simple? g))
(define-generic (graph-indegree g))
(define-generic (graph-outdegree g))
(define-generic (graph-degree g))

;;; Below are the default methods that aren't affected by specialization in graph type
;;; The reason for putting them here is to distinguish functionality based on type by
;;; separating the source files.

(define-method (graph-attribute (g <abstract-graph>) keyword)
  (hash-table-ref (graph-attr g)
                  keyword))

(define-method (graph-attribute-set! before: (g <abstract-graph>) keyword value)
  (unless (keyword? keyword)
    (error 'graph-attribute-set!
           "Second argument must be a keyword (e.g. 'name:' without quotes), not just a symbol"
           keyword)))

(define-method (graph-attribute-set! (g <abstract-graph>) keyword value)
  (hash-table-set! (graph-attr g) keyword value))

(define-method (graph-attribute-set before: (g <abstract-graph>) keyword value)
  (unless (keyword? keyword)
    (error 'graph-attribute-set
           "Second argument must be a keyword (e.g. 'name:' without quotes), not just a symbol"
           keyword)))

(define-method (graph-attribute-set (g <abstract-graph>) keyword value)
  (let ([new-graph (graph-copy g)])
   (graph-attribute-set! new-graph keyword value)
   new-graph))

(define-method (graph-vertex-exists? (g <abstract-graph>) v)
    @("Tests whether vertex v exists in graph g"
      (g "The graph to test")
      (v "The vertex to search for")
      (@to "bool")
      (@no-source))
    (let ([data (adjacency-table g)])
     (hash-table-exists? data v)))

(define-method (graph-vertex before: (g <abstract-graph>) vertex)
  (unless (graph-vertex-exists? g vertex)
    (error 'graph-vertex
           "Cannot query vertex - does not exist" vertex)))

(define-method (graph-vertex (g <abstract-graph>) vertex)
  (hash-table->alist (hash-table-ref (graph-vertex-attr g) vertex)))

(define-method (graph-vertex-add before: (g <abstract-graph>) vertex #!rest attr)
  (if (graph-vertex-exists? g vertex)
    (error 'graph-vertex-add "Vertex already exists" vertex)))

(define-method (graph-vertex-add (g <abstract-graph>) vertex #!rest attr)
  @("Adds a vertex with no edge connections to a graph (Non-destructive)"
    "If the vertex already exists, an error is raised"
    (g "The graph to add a vertex to")
    (vertex "The identifier for the vertex to add. (e.g. symbol or integer)")
    (@to "(type-of g)")
    (@example
      "Adds the vertex b to the graph G"
      (define G '((a (c))
                  (c (a))))
      (graph->alist (graph-vertex-add G 'b)))
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (vertex-add! new-graph vertex attr)
   new-graph))

(define-method (graph-vertex-add! before: (g <abstract-graph>) vertex #!rest attr)
  (if (graph-vertex-exists? g vertex)
    (error 'graph-vertex-add! "Vertex already exists" vertex)))

(define-method (graph-vertex-add! (g <abstract-graph>) vertex #!rest attr)
  @("Adds a vertex with no edge connections to a graph (Destructive)"
    (g "The graph to add a vertex to")
    (vertex "The identifier for the vertex to add. (e.g. symbol or integer)")
    (@example
      "Adds the vertex b to the graph G"
      (define G '((a (c))
                  (c (a))))
      (graph-vertex-add! G 'b)
      (graph->alist G))
    (@no-source))
  (vertex-add! g vertex attr))

(define-method (graph-vertex-remove before: (g <abstract-graph>) vertex)
  (if (not (graph-vertex-exists? g vertex))
    (error 'graph-vertex-remove "Vertex does not exist in graph" vertex)))

(define-method (graph-vertex-remove (g <abstract-graph>) vertex)
  @("Removes a vertex (and all associated edge references) from a graph. (Non-destructive)"
    "First checks if a vertex exists, an raises an error if it does not."
    (g "The graph to remove the vertex from")
    (vertex "The vertex to remove")
    (@to "(type-of g")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (vertex-remove! new-graph vertex)
   new-graph))

(define-method (graph-vertex-remove! before: (g <abstract-graph>) vertex)
  (if (not (graph-vertex-exists? g vertex))
    (error 'graph-vertex-remove!  "Vertex does not exist in graph" vertex)))

(define-method (graph-vertex-remove! (g <abstract-graph>) vertex)
  @("Removes a vertex (and all associated edge references) from a graph. (Non-destructive)"
    "First checks if a vertex exists, an raises an error if it does not."
    (g "The graph to remove the vertex from")
    (vertex "The vertex to remove")
    (@no-source))
  (vertex-remove! g vertex))

(define-method (graph-vertex-update before: (g <abstract-graph>) vertex #!rest attr)
  (if (not (graph-vertex-exists? g vertex))
    (error 'graph-vertex-update
           "Cannot update vertex - does not exist" vertex)))

(define-method (graph-vertex-update (g <abstract-graph>) vertex #!rest attr)
  @("Updates the attribute(s) for a vertex in g. (Non-destructive)"
    (g "The graph with which the vertex resides.")
    (vertex "The vertex of which to update the attributes of.")
    (attr "A list of keywords and their corresponding values that define attributes for the vertex.")
    (@to "(type-of g)")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (vertex-update! new-graph vertex attr)
   new-graph))

(define-method (graph-vertex-update! (g <abstract-graph>) vertex #!rest attr)
  (if (not (graph-vertex-exists? g vertex))
    (error 'graph-vertex-update!
           "Cannot update vertex - does not exist" vertex)))

(define-method (graph-vertex-update! (g <abstract-graph>) vertex #!rest attr)
  @("Updates the attribute(s) for a vertex in g. (Destructive)"
    (g "The graph with which the vertex resides.")
    (vertex "The vertex of which to update the attributes of.")
    (attr "A list of keywords and their corresponding values that define attributes for the vertex.")
    (@no-source))
  (vertex-update! g vertex attr))

(define-method (graph-indegree (g <abstract-graph>) u)
  @("Calculates the indegree of a vertex u in graph g."
    (g "The graph which we calculate the indegree within")
    (u "The vertex of which we are interested in the indegree of.")
    (@to "number")
    (@no-source))
  (let ([data (adjacency-table g)])
   (hash-table-fold data
                    (lambda (key val knil)
                      (let ([ws (set-filter (lambda (x)
                                              (equal? u (car x)))
                                            val)])
                        (if (not (set-null? ws))
                          (apply +
                                 knil
                                 (map (lambda (x)
                                        (hash-table-ref/default (cdr x)
                                                                weight:
                                                                1))
                                      (set->list ws)))
                          knil)))
                    0)))

(define-method (graph-outdegree (g <abstract-graph>) u)
  @("Calculates the outdegree of a vertex u in graph g."
    (g "The graph which we calculate the outdegree within")
    (u "The vertex of which we are interested in the outdegree of.")
    (@to "number")
    (@no-source))
  (set-count (graph-neighbours g u)))

(define-method (graph-degree (g <abstract-graph>) u)
  @("Calculates the overall degree of the vertex u in graph g."
    "Represents the sum of the indegree and outdegrees of vertex u in graph g."
    "NOTE: if the graph is weighted then the weighted indegree is calculated."
    (g "The graph which we calculate the degree within")
    (u "The vertex of which we are interested in the degree of.")
    (@to "number")
    (@no-source))
  (+ (graph-indegree g u)
     (graph-outdegree g u)))
