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

;;; A set of low-level procedures for directly editing the adjacency tables
;;; of a graph or for performing common procedures (such as vertex-remove, etc).

(define (loops? adjacency-table)
  (call/cc
    (lambda (k)
      (hash-table-for-each adjacency-table
                           (lambda (key val)
                             (when (set-in key (set-map car val))
                               (k #t))))
      #f)))

(define (multiple-edges? adjacency-table)
  (call/cc
    (lambda (k)
      (hash-table-for-each adjacency-table
                           (lambda (key val)
                             (when (> (set-count val)
                                      (set-count (set-map car val)))
                               (k #t))))
      #f)))

(define (vertex-add! g vertex attr)
  (hash-table-set! (adjacency-table g) vertex (list->set '()))
  (hash-table-set! (graph-vertex-attr g)
                   vertex
                   (hash-table-from-keys attr)))

(define (vertex-remove! g vertex)
  (let ([data (adjacency-table g)]
        [vattr (graph-vertex-attr g)])
    (hash-table-remove! data (lambda (key val)
                               (equal? vertex key)))
    (hash-table-remove! vattr (lambda (key val)
                                (equal? vertex key)))
    (hash-table-for-each data
                         (lambda (key val)
                           (let ([new-val (filter (lambda (x)
                                                    (equal? vertex (car x)))
                                                  (set->list val))])
                             (hash-table-set! data
                                              key
                                              (set-difference val (list->set new-val))))))))

(define (vertex-update! g vertex attr)
  (let ([attr-table (hash-table-ref (graph-vertex-attr g) vertex)])
   (hash-table-set! (graph-vertex-attr g)
                    vertex
                    (hash-table-merge! (hash-table-from-keys attr)
                                       attr-table))))

(define (edge-add! g u v attr)
  (let ([data (adjacency-table g)])
   (hash-table-set! data
                    u
                    (set-union (set-filter (lambda (x)
                                             (not (equal? v (car x))))
                                           (hash-table-ref data u))
                               (set (cons v (hash-table-from-keys attr)))))))

(define (multiedge-add! g u v attr)
  (let ([data (adjacency-table g)])
   (hash-table-set! data
                    u
                    (set-union (hash-table-ref data u)
                               (set (cons v (hash-table-from-keys attr)))))))

(define (edge-remove! g u v)
  (let ([data (adjacency-table g)])
   (hash-table-set! data u (list->set (filter (lambda (x)
                                                (not (equal? x v)))
                                              (set->list (graph-neighbours g u)))))))

(define (multiedge-remove! g u v id)
  (let* ([data (adjacency-table g)]
         [edge-attr (set-find (lambda (x)
                                (and (equal? (car x) v)
                                     (equal? (hash-table-ref (cdr x) id:)
                                             id)))
                              (hash-table-ref data u))])
    (if edge-attr
      (hash-table-set! data u (set-filter (lambda (x)
                                            (not (equal? x edge-attr)))
                                          (hash-table-ref data u))))))

(define (edge-update! g u v attr)
  (let* ([data (adjacency-table g)]
         [e    (set-find (lambda (x)
                           (equal? (car x) v))
                         (hash-table-ref data u))]
         [edge-attr (cdr e)])
    (edge-remove! g u v)
    (edge-add! data
               u
               v
               (hash-table-merge! (hash-table-from-keys attr)
                                  edge-attr))))

(define (multiedge-update! g u v id attr)
  (when (any (cute eq? id: <>) attr)
    (error 'multiedge-update!
           "Cannot update ID attribute once set. Remove edge then add again"
           attr))
  (let* ([data (adjacency-table g)]
         [edges (set-filter (lambda (x)
                              (equal? (car x) v))
                            (hash-table-ref data u))]
         [edge-attrs (set-map cdr edges)])
    (set-for-each (lambda (eattr)
                    (when (or (not id)
                              (equal? id
                                      (hash-table-ref eattr id:)))
                      (multiedge-remove! g u v id)
                      (hash-table-set! data
                                       u
                                       (set-union (hash-table-ref data u)
                                                  (set (cons v
                                                             (hash-table-merge! (hash-table-from-keys attr)
                                                                                eattr)))))))
                  edge-attrs)))

