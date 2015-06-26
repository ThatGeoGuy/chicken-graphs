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

;;; NOTE: Bear in mind all the functions in src/utils.scm need to be defined
;;; in order for the methods / procedures defined here to work.

(define (make-digraph #!rest attr)
  (make <digraph>
        'gattr (parse-graph-attr attr)))

(define-method (graph-neighbours (g <digraph>) u)
  @("Returns the set of vertices v such that there is an edge from u to v."
    (g "The graph to check.")
    (u "The head vertex.")
    (v "The tail vertex.")
    (@to "set")
    (@no-source))
  (set-map car (hash-table-ref (adjacency-table g) u)))

(define-method (print-object (object <digraph>) port)
  (display "Digraph\n" port)
  (display "-------\n" port)
  (hash-table-for-each (graph-vertex-attr object)
                       (lambda (key val)
                         (display key port)
                         (display " |-> " port)
                         (display (graph-neighbours object key) port))))

(define-method (graph-copy (g <digraph>))
  @("Creates a copy of the data in digraph g"
    "Because attributes are stored as hash-tables, only a shallow copy of each of these hash tables are made."
    (g "The digraph to copy")
    (@to "<digraph>")
    (@no-source))
  (make <digraph>
        'gattr (hash-table-copy (graph-attributes g))
        'vattr (hash-table-copy (graph-vertex-attr g))
        'atbl (hash-table-copy (adjacency-table g))))

(define-method (graph-adjacent? (g <digraph>) u v)
  @("Tests whether an edge u->v exists in digraph g"
    (g "The digraph to test")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "bool")
    (@no-source))
  (cond
    [(not (and (graph-vertex-exists? g u)
               (graph-vertex-exists? g v)))
     #f]
    [(set-in v (graph-neighbours g u)) #t]
    [else #f]))

(define-method (graph-edge-add before: (g <digraph>) u v #!rest attr)
  (when (graph-adjacent? g u v)
    (error 'graph-edge-add "Cannot add new edge - already exists" u v)))

(define-method (graph-edge-add (g <digraph>) u v #!rest attr)
  @("Adds an edge u->v that does not already exist within the digraph g. (Non-destructive)"
    "First checks if the edge exists within the digraph. If it does, it raises an error."
    (g "The digraph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (attr "A list of keywords and their corresponding values that define attributes for the edge.")
    (@to "<digraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (unless (graph-vertex-exists? new-graph u)
     (graph-vertex-add! new-graph u))
   (unless (graph-vertex-exists? new-graph v)
     (graph-vertex-add! new-graph v))
   (edge-add! new-graph u v attr)
   new-graph))

(define-method (graph-edge-add! before: (g <digraph>) u v #!rest attr)
  (when (graph-adjacent? g u v)
    (error 'graph-edge-add! "Cannot add new edge - already exists" u v)))

(define-method (graph-edge-add! (g <digraph>) u v #!rest attr)
  @("Adds an edge u->v that does not already exist within the digraph g. (Destructive)"
    "First checks if the edge exists within the digraph. If it does, it raises an error."
    (g "The digraph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (attr "A list of keywords and their corresponding values that define attributes for the edge.")
    (@no-source))
  (unless (graph-vertex-exists? g u)
    (graph-vertex-add! g u))
  (unless (graph-vertex-exists? g v)
    (graph-vertex-add! g v))
  (edge-add! g u v attr))

(define-method (graph-edge-remove before: (g <digraph>) u v)
  (unless (graph-adjacent? g u v)
    (error 'graph-edge-remove "Cannot remove edge - doesn't exist" u v)))

(define-method (graph-edge-remove (g <digraph>) u v)
  @("Removes an edge u->v from a digraph. (Non-destructive)"
    "First checks if the edge exists within the digraph. If it does not, it raises an error."
    (g "The graph to remove the edge from")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<digraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (edge-remove! new-graph u v)
   new-graph))

(define-method (graph-edge-remove! before: (g <digraph>) u v)
  (unless (graph-adjacent? g u v)
    (error 'graph-edge-remove! "Cannot remove edge - doesn't exist" u v)))

(define-method (graph-edge-remove! (g <digraph>) u v)
  @("Removes an edge u->v from a digraph. (Destructive)"
    "First checks if the edge exists within the digraph. if it does not, it raises an error."
    "If the graph is undirected, it removes u->v and v->u."
    (g "The graph to remove the edge from")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<digraph>")
    (@no-source))
  (edge-remove! g u v))

(define-method (graph-edge-update before: (g <digraph>) u v #!rest attr)
    (unless (graph-adjacent? g u v)
      (error 'graph-edge-update "Cannot update edge, does not exist" u v)))

(define-method (graph-edge-update (g <digraph>) u v #!rest attr)
  @("Updates an edge u->v present within a digraph g. (Non-destructive)"
    "First checks if the edge exists within the digraph. if it does not, it raises an error."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<digraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (edge-update! g u v attr)
   new-graph))

(define-method (graph-edge-update! before: (g <digraph>) u v #!rest attr)
  (unless (graph-adjacent? g u v)
    (error 'graph-edge-update! "Cannot update edge, does not exist" u v)))

(define-method (graph-edge-update! (g <digraph>) u v #!rest attr)
  @("Updates an edge u->v present within a graph g. (Destructive)"
    "First checks if the edge exists within the digraph. if it does not, it raises an error."
    "If the graph is undirected, then u->v and v->u are updated."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (@no-source))
  (edge-update! g u v attr))

(define-method (graph-simple? (g <digraph>))
  @("Predicate to evaluate if a graph or digraph is simple."
    "A graph/digraph is simple if and only if there are no self loops between vertices and no multiple edges."
    (g "The graph to evaluate")
    (@to "bool")
    (@no-source))
  (let ([data (adjacency-table g)])
   (loops? data)))
