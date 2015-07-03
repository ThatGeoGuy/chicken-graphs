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
     posix
     test
     test-generative)
(import sets)

;; Every test needs to load the module first
;; The module should be useable without having to install it first
(use graphs)

(define (gen-vertex-obj)
  (gen-sample-of
    (lambda ()
      (string->symbol
        ((gen-string-of (gen-char #\0 #\z)
                        (lambda () 8)))))
    (gen-string-of (gen-char #\0 #\z)
                   (lambda () 8))
    (gen-fixnum)))

(include "test-digraph.scm")
(include "test-graph.scm")
(include "test-multidigraph.scm")
(include "test-multigraph.scm")
(test-exit)
