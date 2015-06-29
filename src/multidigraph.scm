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
;;;

(define (make-multidigraph #!rest attr)
  (make <multidigraph>
        'gattr (parse-graph-attr attr)))

(define-method (graph-neighbours (g <multidigraph>) u)
  @("Returns the set of vertices v such that there is an edge from u to v."
    (g "The multigraph to check.")
    (u "The head vertex.")
    (v "The tail vertex.")
    (@to "set")
    (@no-source))
  (set-map (lambda (x)
             (list (car x)
                   (hash-table-ref (cdr x) id:)))
           (hash-table-ref (adjacency-table g) u)))

(define-method (print-object (object <multidigraph>) port)
  (display "MultiDigraph\n" port)
  (display "------------\n" port)
  (hash-table-for-each (graph-vertex-attr object)
                       (lambda (key val)
                         (display key port)
                         (display " |-> " port)
                         (display (graph-neighbours object key) port))))

(define (digraph? g)
  @("Type predicate for digraphs"
    (g "The object to test")
    (@to "bool")
    (@no-source))
  (let ([class (class-of g)])
   (or (eq? class <digraph>)
       (eq? class <multidigraph>))))

(define (multigraph? g)
  @("Type predicate for multigraphs"
    (g "The object to test")
    (@to "bool")
    (@no-source))
  (let ([class (class-of g)])
   (subclass? class <multidigraph>)))

(define-method (graph-copy (g <multidigraph>))
  @("Creates a copy of the data in graph g"
    "Because attributes are stored as hash-tables, only a shallow copy of each of these hash tables are made."
    (g "The graph to copy")
    (@to "<multidigraph>")
    (@no-source))
  (make <multidigraph>
        'gattr (hash-table-copy (graph-attr g))
        'vattr (hash-table-copy (graph-vertex-attr g))
        'atbl (hash-table-copy (adjacency-table g))))

(define-method (graph-adjacent? (g <multidigraph>) u v #!optional (id #f))
  @("Tests whether an edge u->v and v->u exists in graph g"
    (g "The multidigraph to test")
    (u "The head vertex")
    (v "The tail vertex")
    (id "The ID of the edge to check for")
    (@to "bool")
    (@no-source))
  (cond
    [(not (and (graph-vertex-exists? g u)
               (graph-vertex-exists? g v)))
     #f]
    [(set-in v (set-map car (graph-neighbours g u)))
     (if id
       (set-map cadr (graph-neighbours g u))
       #t)]
    [else #f]))

(define-method (graph-edge before: (g <multidigraph>) u v id)
  (unless (graph-adjacent? g u v id)
    (error 'graph-edge "Cannot query edge - does not exist" u v id)))

(define-method (graph-edge (g <multidigraph>) u v id)
  (let* ([edges (hash-table-ref (adjacency-table g) u)]
         [e (set-find (lambda (x)
                        (and (equal? (car x) v)
                             (equal? (hash-table-ref (cdr x)
                                                     id:)
                                     id)))
                      edges)])
    (hash-table->alist (cdr e))))

(define-method (graph-edge-add before: (g <multidigraph>) u v id #!rest attr)
  (when (graph-adjacent? g u v id)
    (error 'graph-edge-add
           "Cannot add edge - An edge u->v already exists with provided id"
           u v id)))

(define-method (graph-edge-add (g <multidigraph>) u v id #!rest attr)
  @("Adds an edge u->v that does not already exist within the graph g. (Non-destructive)"
    "If the graph is not a digraph, also adds v->u to balance the graph."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (id "Unique identifier for the edge (to differentiate it from other edges from u->v)")
    (attr "A list of keywords and their corresponding values that define attributes for the edge.")
    (weight "Specific keyword in the edge attributes that corresponds to the edge weight.")
    (@to "<multidigraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)]
        [attr (cons id: (cons id attr))])
    (unless (graph-vertex-exists? new-graph u)
      (graph-vertex-add! new-graph u))
    (unless (graph-vertex-exists? new-graph v)
      (graph-vertex-add! new-graph v))
    (multiedge-add! new-graph u v attr)
    new-graph))

(define-method (graph-edge-add! before: (g <multidigraph>) u v id #!rest attr)
  (when (graph-adjacent? g u v id)
    (error 'graph-edge-add!
           "Cannot add edge - An edge u->v already exists with provided id"
           u v id)))

(define-method (graph-edge-add! (g <multidigraph>) u v id #!rest attr)
  @("Adds an edge u->v that does not already exist within the graph g. (Destructive)"
    "If the graph is not a digraph, also adds v->u to balance the graph."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (id "Unique identifier for the edge (to differentiate it from other edges from u->v)")
    (attr "A list of keywords and their corresponding values that define attributes for the edge.")
    (weight "Specific keyword in the edge attributes that corresponds to the edge weight.")
    (@no-source))
  (let ([attr (cons id: (cons id attr))])
   (if (not (graph-vertex-exists? g u))
     (graph-vertex-add! g u))
   (if (not (graph-vertex-exists? g v))
     (graph-vertex-add! g v))
   (multiedge-add! g u v attr)))

(define-method (graph-edge-remove before: (g <multidigraph>) u v #!optional (id #f))
  (unless (graph-adjacent? g u v id)
    (error 'graph-edge-remove
           "Cannot remove edge - No edge from u->v exists with id"
           u v id)))

(define-method (graph-edge-remove (g <multidigraph>) u v #!optional (id #f))
  @("Removes an edge u->v and v->u with identity `id` from a graph. (Non-destructive)"
    "If no edge with the corresponding ID can be found, an error is raised."
    "If ID is false, then all edges u->v (and v->u) are removed."
    (g "The multidigraph to remove the edge from.")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<multidigraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (cond
     [id (multiedge-remove! new-graph u v id)]
     [else (edge-remove! new-graph u v)])
   new-graph))

(define-method (graph-edge-remove! before: (g <multidigraph>) u v #!optional (id #f))
  (unless (graph-adjacent? g u v id)
    (error 'graph-edge-remove!
           "Cannot remove edge - No edge from u->v exists with id"
           u v id)))

(define-method (graph-edge-remove! (g <multidigraph>) u v #!optional (id #f))
  @("Removes an edge u->v and v->u with identity `id` from a graph. (Destructive)"
    "If no edge with the corresponding ID can be found, an error is raised."
    "If ID is false, then all edges u->v (and v->u) are removed."
    (g "The multidigraph to remove the edge from.")
    (u "The head vertex")
    (v "The tail vertex")
    (@no-source))
  (cond
    [id (multiedge-remove! g u v id)]
    [else (edge-remove! g u v)]))

(define-method (graph-edge-update before: (g <multidigraph>) u v id #!rest attr)
    (unless (graph-adjacent? g u v id)
      (error 'graph-edge-update "Cannot update edge, does not exist" u v id)))

(define-method (graph-edge-update (g <multidigraph>) u v id #!rest attr)
  @("Updates the edges u->v and v->u with ID := `id` present within a multidigraph g. (Non-destructive)"
    "If `id` is #f then all edges u->v and v->u are updated."
    (g "The graph of which to update")
    (u "The base vertex")
    (v "The extended vertex")
    (@to "<graph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (multiedge-update! new-graph u v id attr)
   new-graph))

(define-method (graph-edge-update! before: (g <graph>) u v id #!rest attr)
  (unless (graph-adjacent? g u v id)
    (error 'graph-edge-update! "Cannot update edge, does not exist" u v id)))

(define-method (graph-edge-update! (g <graph>) u v id #!rest attr)
  @("Updates the edges u->v with ID := `id` present within a multidigraph g. (Destructive)"
    "If `id` is #f then all edges u->v are updated."
    (u "The base vertex")
    (v "The extended vertex")
    (@no-source))
  (multiedge-update! g u v attr))

(define-method (graph-simple? (g <multidigraph>))
  @("Predicate to evaluate if a multigraph is simple."
    "A graph is simple if and only if there are no self loops between vertices and no multiple edges."
    (g "The graph to evaluate")
    (@to "bool")
    (@no-source))
  (let ([data (adjacency-table g)])
   (not (or (multiple-edges? data)
            (loops? data)))))
