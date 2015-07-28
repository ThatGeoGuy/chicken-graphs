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
;;;     src/digraph
;;;

(define (make-graph #!rest attr)
  (make <graph>
        'gattr (parse-graph-attr attr)))

(define-method (print-object (object <graph>) port)
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
        'gattr (hash-table-copy (graph-attr g))
        'vattr (hash-table-copy (graph-vertex-attr g))
        'atbl (hash-table-copy (adjacency-table g))))

(define (graph? g)
  @("Type predicate for graphs of any variety"
    (g "The graph to test")
    (@to "bool")
    (@no-source))
  (subclass? (class-of g) <abstract-graph>))

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
   (apply graph-edge-add! new-graph u v attr)
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
   (graph-edge-remove! new-graph u v)
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
   (apply graph-edge-update! new-graph u v attr)
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

(define-method (graph-degree (g <graph>) u #!key (weighted #t))
  (graph-outdegree g u weighted: weighted))
