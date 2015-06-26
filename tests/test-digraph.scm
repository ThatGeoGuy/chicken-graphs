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

(use arrays
     data-generators
     test
     test-generative)
(import sets)

;; Every test needs to load the module first
;; The module should be useable without having to install it first
(load-relative "../chicken-graphs")
(import graphs)

(define (gen-vertex-obj)
  (gen-sample-of
    (lambda ()
      (string->symbol
        ((gen-string-of (gen-char #\0 #\z)
                        (lambda () 8)))))
    (gen-string-of (gen-char #\0 #\z)
                   (lambda () 8))
    (gen-fixnum)))

(test-begin "Digraph Properties")
(test-group "identity properties"
  (test-generative ([v (gen-vertex-obj)])
    (let* ([G1 (make-digraph)]
           [G2 (graph-vertex-remove (graph-vertex-add G1 v) v)])
      (test-assert "Vertex identity - adjacency lists should be empty"
        (and (equal? (graph->list G1) '())
             (equal? (graph->list G2) '())))))

  (test-generative ([u (gen-vertex-obj)]
                    [v (gen-vertex-obj)])
    (let* ([G1 (make-digraph)]
           [G2 (graph-edge-remove (graph-edge-add G1 u v) u v)])
      (test-assert "Edge identity - v should not be adjacent to u"
        (not (graph-adjacent? G2 u v)))
      (test-assert "Edge identity - u should not be adjacent to v"
        (not (graph-adjacent? G2 v u)))
      (test-assert "Edge identity - neighbours of u should be empty set"
        (set-null? (graph-neighbours G2 u)))
      (test-assert "Edge identity - neighbours of v should be empty set"
        (set-null? (graph-neighbours G2 v)))
      (test-assert "Edge identity - vertex u should still exist despite edge removal"
        (graph-vertex-exists? G2 u))
      (test-assert "Edge identity - vertex v should still exist despite edge removal"
        (graph-vertex-exists? G2 v))
      (test-assert "Edge identity - original graph should not contain u->v"
        (not (graph-adjacent? G1 u v)))
      (test-assert "Edge identity - original graph should not have u or v vertices"
        (not (or (graph-vertex-exists? G1 u)
                 (graph-vertex-exists? G1 v)))))))

(test-group "commutativity properties"
  (test-generative ([u (gen-vertex-obj)]
                    [v (gen-vertex-obj)])
    (unless (equal? u v)
      (let* ([G1 (make-digraph)]
             [G2 (graph-vertex-add (graph-vertex-add G1 u) v)]
             [G3 (graph-vertex-add (graph-vertex-add G1 v) u)]
             [G4 (graph-vertex-remove (graph-vertex-remove G2 u) v)]
             [G5 (graph-vertex-remove (graph-vertex-remove G3 v) u)])
        (test-assert "Vertex commutativity - u should be present in both graphs"
          (and (graph-vertex-exists? G2 u)
               (graph-vertex-exists? G3 u)))
        (test-assert "Vertex commutativity - v should be present in both graphs"
          (and (graph-vertex-exists? G2 v)
               (graph-vertex-exists? G3 v)))
        (test-assert "Vertex commutativity - order of removal should not matter"
          (and (not (or (graph-vertex-exists? G4 u)
                        (graph-vertex-exists? G4 v)))
               (not (or (graph-vertex-exists? G5 u)
                        (graph-vertex-exists? G5 v)))))))

    (let* ([G1 (make-digraph)]
           [G2 (graph-edge-add G1 u v)]
           [G3 (graph-edge-add G1 v u)]
           [G4 (graph-edge-remove G2 u v)]
           [G5 (graph-edge-remove G3 v u)])
      (test-assert "Edge commutativity - u should be adjacent to v"
        (graph-adjacent? G2 u v))
      (test-assert "Edge commutativity - v should be adjacent to u"
        (graph-adjacent? G3 v u))
      (test-assert "Edge commutativity - removing edge u<->v: order should not matter"
        ;; All tests are still valid since no edges should be present at all
        (not (or (graph-adjacent? G4 u v)
                 (graph-adjacent? G4 v u)
                 (graph-adjacent? G5 u v)
                 (graph-adjacent? G5 v u)))))))

(test-end "Digraph Properties")
