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
