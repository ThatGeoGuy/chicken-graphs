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

;;; NOTE: Bear in mind all the procedures in the following files must first be
;;; defined in the graphs module:
;;;
;;;     src/utils
;;;     src/low-level
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
       (set-in id (set-map cadr (set-filter (lambda (x)
                                              (equal? (car x) v))
                                            (graph-neighbours g u))))
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
    (hash-table-copy (cdr e))))

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
  (let ([new-graph (graph-copy g)])
    (apply graph-edge-add! new-graph u v id attr)
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
   (unless (graph-vertex-exists? g u)
     (graph-vertex-add! g u))
   (unless (graph-vertex-exists? g v)
     (graph-vertex-add! g v))
   (multiedge-add! g u v attr)))

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
   (graph-edge-remove! new-graph u v id)
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

(define-method (graph-edge-update (g <multidigraph>) u v id #!rest attr)
  @("Updates the edges u->v and v->u with ID := `id` present within a multidigraph g. (Non-destructive)"
    "If `id` is #f then all edges u->v and v->u are updated."
    (g "The graph of which to update")
    (u "The base vertex")
    (v "The extended vertex")
    (@to "<graph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (apply graph-edge-update! new-graph u v id attr)
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
  (multiedge-update! g u v id attr))

(define-method (graph-simple? (g <multidigraph>))
  @("Predicate to evaluate if a multigraph is simple."
    "A graph is simple if and only if there are no self loops between vertices and no multiple edges."
    (g "The graph to evaluate")
    (@to "bool")
    (@no-source))
  (let ([data (adjacency-table g)])
   (and (not (multiple-edges? data))
        (not (loops? data)))))
