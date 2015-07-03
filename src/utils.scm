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

(define (all? pred? ls)
  @("Checks if pred? is true for all items in the list ls"
    (pred? "The predicate to check")
    (ls "The list of elements with which to check the predicate for")
    (@to "bool")
    (@example
      "Check if all numbers in the list are odd."
      (all? odd? '(1 3 5 7 8)))
    (@no-source)
    (@internal))
  (call/cc (lambda (k)
             (for-each (lambda (x)
                         (unless (pred? x)
                           (k #f)))
                       ls)
             #t)))

(define (set-find pred? aset)
  @("Searches the set for the first item that satisfies `pred?`, or returns #f if none are found."
    "Since sets are unordered, there is no guarantee that this returns the same object every time if multiple objects satisfy `pred?`"
    (pred? "A function of 1-arity that tests some property about items in set `aset`")
    (aset "A set to find objects in.")
    (@no-source)
    (@internal))
  (call/cc
    (lambda (k)
      (set-for-each (lambda (item)
                      (when (pred? item)
                        (k item)))
                    aset)
      (k #f))))

(define (set-fold f knil aset)
  @("Folds over all the items in the set, calling (f knil item) for each item in the set."
    "Note that there is no necessary order to a set, so left and right folds are not distinguished."
    (f "A function of 2-arity (knil item) to fold over all the items in set.")
    (knil "A starting point to fold over.")
    (aset "A set to fold over.")
    (@example
      "Sums all the unique numbers in the set"
      (define aset (set 1 2 2 3 3 4 4 4 5))
      (set-fold + 0 aset))
    (@no-source))
  (let ([knil knil])
   (set-for-each (lambda (item)
                   (set! knil (f knil item)))
                 aset)
   knil))

(define (hash-table-from-keys ls)
  @("Constructs a hash-table (as provided by srfi-69) from a key-value list."
    "The list is composed of (key: value ...) pairs, where each keyword is used to reference each value."
    "Errors are raised if there are stray values or if there exists a key without a value."
    (@to "hash-table")
    (@example
      "Creates a hash table matching each value of the alphabet to a number"
      (define letters (map string
                           (string->list "abcdefghijklmnopqrstuvwxyz")))
      (define key-val-pairs (concatenate
                              (zip (map string->keyword letters)
                                   (iota 26 1))))
      (hash-table-from-keys key-val-pairs))
    (@no-source)
    (@internal))
  (let loop ([ls ls]
             [table (make-hash-table test: equal?)])
    (match ls
      [() table]
      [((? keyword? k) v rest ...)
       (hash-table-set! table k v)
       (loop rest table)]
      [((? (compose not keyword?) k) rest ...)
       (error 'hash-table-from-keys "Stray value in key-val list" k)]
      [((? keyword? k))
       (error 'hash-table-from-keys "Key with no value" (keyword->string k))])))

(define (parse-graph-attr attr)
  @("Checks the type of attr when passed into a make-*graph function and returns a hash-table of attributes."
    (attr "An alist or hash-table of attributes that correspond to the graph overall")
    (@to "hash-table")
    (@no-source)
    (@internal))
  (match attr
    [((k . v) ...)
     (alist->hash-table attr test: equal?)]
    [(? hash-table? attr) attr]
    [_ (hash-table-from-keys attr)]))

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
  (let ([data (adjacency-table g)])
   (hash-table-remove! data (lambda (key val)
                              (equal? vertex key)))
   (hash-table-remove! data (lambda (key val)
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

