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

(test-begin "Isomorphism")

(test-group "Isomorphic Identity"
  (test-generative ([G (gen-graph)])
    (test-assert "Any graph should be isomorphic to itself"
      (graph-isomorphic? G G))))

(test-group "Isomorphic Associativity"
  (test-generative ([G1 (gen-graph 5)]
                    [G2 (gen-graph 5)]
                    [G3 (gen-graph 5)])
    (test-assert "If G1 <=> G2 and G2 <=> G3 then G1 <=> G3 else G1 !=> G3"
      (cond
        [(and (graph-isomorphic? G1 G2)
              (graph-isomorphic? G2 G3))
         (graph-isomorphic? G1 G3)]
        [(xor (graph-isomorphic? G1 G2)
              (graph-isomorphic? G2 G3))
         (not (graph-isomorphic? G1 G3))]
        ;; The else case is ambiguous; if G1 and G2 are not isomorphic
        ;; to G3, then there is no guarantee that G1 is not isomorphic
        ;; to G3.
        [else #t]))))

(test-end "Isomorphism")
