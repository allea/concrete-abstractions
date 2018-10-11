#lang racket

(require test-engine/racket-tests)

(define make-empty-tree
  (lambda () '()))

(define make-nonempty-tree
  (lambda (root left-subtree right-subtree)
    (list root left-subtree right-subtree)))

(define empty-tree? null?)
(define root car)
(define left-subtree cadr)
(define right-subtree caddr)

; exercise 8.6
#|
discuss: this is for the leaf insert
if the node need to be inserted in the middle of the tree, it's more complex:
 root < x < right-root
|#
(define (insert tree x)
  (cond ((empty-tree? tree) (make-nonempty-tree x '() '())) ; x only falls to the leaf node and insert there
        (else
         (if (< x (root tree))
             (make-nonempty-tree (root tree) (insert (left-subtree tree) x) (right-subtree tree))
             (make-nonempty-tree (root tree) (left-subtree tree) (insert (right-subtree tree) x))))
  ))



(check-expect (insert '() 1) '(1 () ()))
(check-expect (insert '(2
                        (1 () ())
                        (4 () ()))
                      3)
              '(2
                (1 () ())
                (4
                 (3 () ())
                 ())))

(test)