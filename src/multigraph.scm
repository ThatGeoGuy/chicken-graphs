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
;;;     src/multidigraph
;;;

(define (make-multigraph #!rest attr)
  (make <multigraph>
        'gattr (parse-graph-attr attr)))

(define-method (print-object (object <multigraph>) port)
  (display "MultiGraph\n" port)
  (display "----------\n" port)
  (hash-table-for-each (graph-vertex-attr object)
                       (lambda (key val)
                         (display key port)
                         (display " |-> " port)
                         (display (graph-neighbours object key) port))))

(define-method (graph-copy (g <multigraph>))
  (make <multigraph>
        'gattr (hash-table-copy (graph-attr g))
        'vattr (hash-table-copy (graph-vertex-attr g))
        'atbl (hash-table-copy (adjacency-table g))))

(define-method (graph-adjacent? (g <multigraph>) u v #!optional (id #f))
  @("Tests whether an edge u->v and v->u exists in graph g"
    (g "The graph to test")
    (u "The head vertex")
    (v "The tail vertex")
    (id "The ID of the edge to check for")
    (@to "bool")
    (@no-source))
  (cond
    [(not (and (graph-vertex-exists? g u)
               (graph-vertex-exists? g v)))
     #f]
    [(and (set-in v (set-map car (graph-neighbours g u)))
          (set-in u (set-map car (graph-neighbours g v))))
     (if id
       (set-in id (set-map cadr (set-filter (lambda (x)
                                              (equal? (car x) v))
                                            (graph-neighbours g u))))
       #t)]
    [(or (set-in v (set-map car (graph-neighbours g u)))
         (set-in u (set-map car (graph-neighbours g v))))
     (error 'graph-adjacent?
            "Undirected graph has unbalanced edges")]
    [else #f]))

(define-method (graph-edge-add (g <multigraph>) u v id #!rest attr)
  @("Adds an edge u->v that does not already exist within the graph g. (Non-destructive)"
    "If the graph is not a digraph, also adds v->u to balance the graph."
    (g "The graph of which to update")
    (u "The head vertex")
    (v "The tail vertex")
    (id "Unique identifier for the edge (to differentiate it from other edges from u->v)")
    (attr "A list of keywords and their corresponding values that define attributes for the edge.")
    (weight "Specific keyword in the edge attributes that corresponds to the edge weight.")
    (@to "<multigraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (apply graph-edge-add! new-graph u v id attr)
   new-graph))

(define-method (graph-edge-add! (g <multigraph>) u v id #!rest attr)
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
   (multiedge-add! g u v attr)
   (unless (equal? u v)
     (multiedge-add! g v u attr))))

(define-method (graph-edge-remove (g <multigraph>) u v #!optional (id #f))
  @("Removes an edge u->v and v->u with identity `id` from a graph. (Non-destructive)"
    "If no edge with the corresponding ID can be found, an error is raised."
    "If ID is false, then all edges u->v (and v->u) are removed."
    (g "The multigraph to remove the edge from.")
    (u "The head vertex")
    (v "The tail vertex")
    (@to "<multigraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (graph-edge-remove! new-graph u v id)
   new-graph))

(define-method (graph-edge-remove! (g <multigraph>) u v #!optional (id #f))
  @("Removes an edge u->v and v->u with identity `id` from a graph. (Destructive)"
    "If no edge with the corresponding ID can be found, an error is raised."
    "If ID is false, then all edges u->v (and v->u) are removed."
    (g "The multigraph to remove the edge from.")
    (u "The head vertex")
    (v "The tail vertex")
    (@no-source))
  (cond
    [id
     (multiedge-remove! g u v id)
     (unless (equal? u v)
       (multiedge-remove! g v u id))]
    [else
     (edge-remove! g u v)
     (unless (equal? u v)
       (edge-remove! g v u))]))

(define-method (graph-edge-update (g <multigraph>) u v id #!rest attr)
  @("Updates the edges u->v and v->u with ID := `id` present within a multigraph g. (Non-destructive)"
    "If `id` is #f then all edges u->v and v->u are updated."
    (g "The graph of which to update")
    (u "The base vertex")
    (v "The extended vertex")
    (@to "<multigraph>")
    (@no-source))
  (let ([new-graph (graph-copy g)])
   (apply graph-edge-update! new-graph u v id attr)
   new-graph))

(define-method (graph-edge-update! (g <multigraph>) u v id #!rest attr)
  @("Updates the edges u->v and v->u with ID := `id` present within a multigraph g. (Destructive)"
    "If `id` is #f then all edges u->v and v->u are updated."
    (g "The graph of which to update")
    (u "The base vertex")
    (v "The extended vertex")
    (@no-source))
  (multiedge-update! g u v id attr)
  (unless (equal? u v)
    (multiedge-update! g v u id attr)))

(define-method (graph-degree (g <multigraph>) u #!key (weighted #t))
  (graph-outdegree g u weighted: weighted))
