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

;;; NOTE: Bear in mind all the procedures in the following files must first be
;;; defined in the graphs module:
;;;
;;;     src/utils
;;;     src/classes
;;;     src/digraph
;;;

(define (make-graph #!rest attr)
  (make <graph>
        'gattr (parse-graph-attr attr)))

(define-method (print-object (object <digraph>) port)
  (display "Graph\n" port)
  (display "-----\n" port)
  (hash-table-for-each (graph-vertex-attr object)
                       (lambda (key val)
                         (display key port)
                         (display " |-> " port)
                         (display (graph-neighbours object key) port))))

(define-method (graph-copy (g <graph>))
  @("Creates a copy of the data in graph g"
    "Because attributes are stored as hash-tables, only a shallow copy of each of these hash tables are made."
    (g "The graph to copy")
    (@to "<graph>")
    (@no-source))
  (make <graph>
        'gattr (hash-table-copy (graph-attributes g))
        'vattr (hash-table-copy (graph-vertex-attr g))
        'atbl (hash-table-copy (adjacency-table g))))

(define (graph? g)
  @("Type predicate for graphs of any variety"
    (g "The graph to test")
    (@to "bool")
    (@no-source))
  (subclass? g <abstract-graph>))

(define-method (graph-adjacent? (g <graph>) u v)
  @("Tests whether an edge u->v and v->u exists in graph g"
    (g "The graph to test")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "bool")
    (@no-source))
  (cond
    [(not (and (graph-vertex-exists? g u)
               (graph-vertex-exists? g v)))
     #f]
    [(and (set-in v (graph-neighbours g u))
          (set-in u (graph-neighbours g v)))
     #t]
    [(or (set-in v (graph-neighbours g u))
         (set-in u (graph-neighbours g v)))
     (error 'graph-adjacent?
            "Undirected graph has unbalanced edges")]
    [else #f]))

(define-method (graph-edge-add (g <graph>) u v #!rest attr)
  @("Adds edges u->v and v->u if they do not already exist within the graph g. (Non-destructive)"
    "First checks if the edge exists within the graph. If it does, it raises an error."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (attr "A list of keywords and their corresponding values that define attributes for the edge.")
    (@to "<graph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (unless (graph-vertex-exists? new-graph u)
     (graph-vertex-add! new-graph u))
   (unless (graph-vertex-exists? new-graph v)
     (graph-vertex-add! new-graph v))
   (edge-add! new-graph u v attr)
   (unless (equal? u v)
     (edge-add! new-graph v u attr))
   new-graph))

(define-method (graph-edge-add! (g <graph>) u v #!rest attr)
  @("Adds edges u->v and v->u if they do not already exist within the graph g. (Destructive)"
    "First checks if the edge exists within the graph. If it does, it raises an error."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (attr "A list of keywords and their corresponding values that define attributes for the edge.")
    (@no-source))
  (when (graph-adjacent? g u v)
    (error 'graph-edge-add! "Cannot add new edge - already exists" u v))
  (unless (graph-vertex-exists? g u)
    (graph-vertex-add! g u))
  (unless (graph-vertex-exists? g v)
    (graph-vertex-add! g v))
  (edge-add! g u v attr)
  (unless (equal? u v)
    (edge-add! g v u attr)))

(define-method (graph-edge-remove (g <graph>) u v)
  @("Removes an edge u->v from a graph. (Non-destructive)"
    "First checks if the edge exists within the graph. If it does not, it raises an error."
    (g "The graph to remove the edge from")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<graph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (edge-remove! new-graph u v)
   (unless (equal? u v)
     (edge-remove! new-graph v u))
   new-graph))

(define-method (graph-edge-remove! (g <graph>) u v)
  @("Removes an edge u->v from a graph. (Destructive)"
    "First checks if the edge exists within the graph. if it does not, it raises an error."
    (g "The graph to remove the edge from")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<graph>")
    (@no-source))
  (edge-remove! g u v)
  (unless (equal? u v)
    (edge-remove! g v u)))

(define-method (graph-edge-update (g <graph>) u v #!rest attr)
  @("Updates an edge u->v present within a graph g. (Non-destructive)"
    "First checks if the edge exists within the graph. if it does not, it raises an error."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<graph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (edge-update! g u v attr)
   (unless (equal? u v)
     (edge-update! g v u attr))
   new-graph))

(define-method (graph-edge-update! (g <graph>) u v #!rest attr)
  @("Updates an edge u->v present within a graph g. (Destructive)"
    "First checks if the edge exists within the graph. if it does not, it raises an error."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (@no-source))
  (edge-update! g u v attr)
  (unless (equal? u v)
    (edge-update! g v u attr)))
