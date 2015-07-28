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
        'gattr (hash-table-copy (graph-attr g))
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

(define-method (graph-edge before: (g <digraph>) u v)
  (unless (graph-adjacent? g u v)
    (error 'graph-edge "Cannot query edge - does not exist" u v)))

(define-method (graph-edge (g <digraph>) u v)
  (let* ([edges (hash-table-ref (adjacency-table g) u)]
         [e (set-find (lambda (x)
                        (equal? (car x) v))
                      edges)])
    (hash-table-copy (cdr e))))

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
   (apply graph-edge-add! new-graph u v attr)
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

(define-method (graph-edge-remove (g <digraph>) u v)
  @("Removes an edge u->v from a digraph. (Non-destructive)"
    "First checks if the edge exists within the digraph. If it does not, it raises an error."
    (g "The graph to remove the edge from")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<digraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (graph-edge-remove! new-graph u v)
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

(define-method (graph-edge-update (g <digraph>) u v #!rest attr)
  @("Updates an edge u->v present within a digraph g. (Non-destructive)"
    "First checks if the edge exists within the digraph. if it does not, it raises an error."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<digraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (apply graph-edge-update! new-graph u v attr)
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
