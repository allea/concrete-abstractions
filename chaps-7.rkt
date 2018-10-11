#lang racket

(require test-engine/racket-tests)

(define (do-subs pattern question)
  (cond ((null? pattern)
         (if (null? question)
             '(())
             'fail)) ; runs out of pattern

        ((null? question) 'fail) ; runs out of question

        ((eq? (car pattern) (car question))
         (do-subs (cdr pattern) (cdr question)))
        
        ((eq? (car pattern) '...)
         (letrec ((pan (lambda (n acc)
                         (cond ((zero? n)
                                acc)
                               (else                              
                                (let ((rest-match (do-subs (cdr pattern) (drop question n))))
                                  (if (eq? rest-match 'fail)
                                      (pan (- n 1) acc)                                   
                                      (pan (- n 1) (append acc
                                                           (map (lambda (s)
                                                                  (cons (take question n) s))
                                                                rest-match)))))                                     
                                )))))
           (pan (- (length question) (length (cdr pattern))) '() )))
        
        (else
         'fail)
        )
  )

(check-expect (do-subs '(a) '(a) ) '(()))
(check-expect (do-subs '(a) '(b) ) 'fail)
(check-expect (do-subs '(a b) '(a) ) 'fail)
(check-expect (do-subs '(a) '(a b) ) 'fail)
(check-expect (do-subs '(a ...) '(a b) ) '(((b))))
(check-expect (do-subs '(a ...) '(a b c) ) '(((b c))))
(check-expect (do-subs '(a ... c) '(a b c) ) '(((b))))
(check-expect (do-subs '(a ... c) '(a b x c) ) '(((b x))))
(check-expect (do-subs '(a ... c ... e) '(a b c d e) ) '(((b) (d))))
(check-expect (do-subs '(a ... c ... e ... g) '(a b c d e f g) ) '(((b) (d) (f))))
(check-expect (do-subs '(do you have ... in ...) '(do you have boyz in the hood in the store))
              '(((boyz in the hood) (the store)) ((boyz) (the hood in the store))))

(test)