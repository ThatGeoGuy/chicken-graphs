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
(define-generic (graph->list g))
(define-generic (graph-attribute g))
(define-generic (graph-attribute-set g))
(define-generic (graph-attribute-set! g))
(define-generic (graph-vertex-exists? g))
(define-generic (graph-adjacent? g))
(define-generic (graph-neighbours g))
(define-generic (graph-vertex g))
(define-generic (graph-vertices g))
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
(define-generic (graph-order g))

;;; Below are the default methods that aren't affected by specialization in graph type
;;; The reason for putting them here is to distinguish functionality based on type by
;;; separating the source files.

(: graph->list deprecated)
(define-method (graph->list (g <abstract-graph>))
  (hash-table-map (adjacency-table g)
                  (lambda (key val)
                    (list key val))))

(define-method (graph-attribute before: (g <abstract-graph>) keyword)
  (unless (hash-table-exists? (graph-attr g) keyword)
    (error 'graph-attribute
           "Attribute does not exist within graph"
           keyword)))

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

(define-method (graph-vertex before: (g <abstract-graph>) vertex)
  (unless (graph-vertex-exists? g vertex)
    (error 'graph-vertex
           "Cannot query vertex - does not exist" vertex)))

(define-method (graph-vertex (g <abstract-graph>) vertex)
  (hash-table-copy (hash-table-ref (graph-vertex-attr g) vertex)))

(define-method (graph-vertices (g <abstract-graph>))
  (list->set (hash-table-keys (graph-vertex-attr g))))

(define-method (graph-vertex-exists? (g <abstract-graph>) v)
  @("Tests whether vertex v exists in graph g"
    (g "The graph to test")
    (v "The vertex to search for")
    (@to "bool")
    (@no-source))
  (set-in v (graph-vertices g)))

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

(define-method (graph-vertex-update! before: (g <abstract-graph>) vertex #!rest attr)
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

(define-method (graph-indegree (g <abstract-graph>) u #!key (weighted #t))
  @("Calculates the indegree of a vertex u in graph g."
    (g "The graph which we calculate the indegree within")
    (u "The vertex of which we are interested in the indegree of.")
    (@to "number")
    (@no-source))
  (let ([data (adjacency-table g)]
        [vertices (set->list (graph-vertices g))])
   (hash-table-fold data
                    (lambda (key val knil)
                      (let ([ws (map (compose (cute hash-table-ref/default
                                                    <> weight: 1)
                                              cdr)
                                     (filter (lambda (x)
                                               (equal? u (car x)))
                                             (set->list val)))])
                        (cond
                          [(and weighted
                                (not (set-null? ws)))
                           (apply + knil ws)]
                          [(not (null? ws))
                           (+ knil (length ws))]
                          [else knil])))
                    0)))

(define-method (graph-outdegree (g <abstract-graph>) u #!key (weighted #t))
  @("Calculates the outdegree of a vertex u in graph g."
    (g "The graph which we calculate the outdegree within")
    (u "The vertex of which we are interested in the outdegree of.")
    (@to "number")
    (@no-source))
  (let* ([out-edges (hash-table-ref (adjacency-table g) u)]
         [ws (map (compose (cute hash-table-ref/default
                                 <> weight: 1)
                           cdr)
                  (set->list out-edges))])
    (cond
      [(and weighted
            (not (set-null? ws)))
       (fold + 0 ws)]
      [(not (set-null? ws))
       (length ws)]
      [else 0])))

(define-method (graph-degree (g <abstract-graph>) u #!key (weighted #t))
  @("Calculates the overall degree of the vertex u in graph g."
    "Represents the sum of the indegree and outdegrees of vertex u in graph g."
    "NOTE: if the graph is weighted then the weighted degree is calculated by default."
    (g "The graph which we calculate the degree within")
    (u "The vertex of which we are interested in the degree of.")
    (weighted "A boolean switch to specify if the weighted degree is desired.")
    (@to "number")
    (@no-source))
  (+ (graph-indegree g u weighted: weighted)
     (graph-outdegree g u weighted: weighted)))

(define-method (graph-order (g <abstract-graph>))
  @("Returns the order of the graph i.e. the number of vertices within the graph."
    (g "The graph to compute the order of.")
    (@to "integer")
    (@no-source))
  (set-count (graph-vertices g)))
