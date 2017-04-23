(use-modules (minikanren unification))
(use-modules (srfi srfi-64))

;; some examples that stress the worst case exponential
;; time aspect of robinsons unification algorithm
;; <http://erikjacobsen.com/pdf/unification.pdf>

;; example 3.1.6

(define (make-list-1-a n)
  (let loop ((i 1))
    (if (= i n)
        '()
        (cons (var i) (loop (+ i 1))))))

(define (make-list-1-b n)
  (let loop ((i 0))
    (if (= i (- n 1))
        '()
        (cons (list (var i) (var i)) (loop (+ i 1))))))

;; example 3.2.1

(test-begin "unification exp-occ")
(test-equal (not (unify
                  (make-list-1-a 10)
                  (make-list-1-b 10)
                  '()))
  #f)
(test-equal (not (unify
                  (make-list-1-a 12)
                  (make-list-1-b 12)
                  '()))
  #f)
(test-equal (not (unify
                  (make-list-1-a 14)
                  (make-list-1-b 14)
                  '()))
  #f)
(test-equal (not (unify
                  (make-list-1-a 16)
                  (make-list-1-b 16)
                  '()))
  #f)
(test-equal (not (unify
                  (make-list-1-a 18)
                  (make-list-1-b 18)
                  '()))
  #f)
(test-equal (not (unify
                  (make-list-1-a 20)
                  (make-list-1-b 20)
                  '()))
  #f)
(test-end "unification exp-occ")


(test-begin "unification exp-comp")
(test-equal (not (unify (list (cons 'p (make-list-1-a 10))
                              (cons 'q (make-list-1-a 10)))
                        (list (cons 'p (make-list-1-b 10))
                              (cons 'q (make-list-1-b 10)))
                        '()))
  #f)
(test-equal (not (unify (list (cons 'p (make-list-1-a 12))
                              (cons 'q (make-list-1-a 12)))
                        (list (cons 'p (make-list-1-b 12))
                              (cons 'q (make-list-1-b 12)))
                        '()))
  #f)
(test-equal (not (unify (list (cons 'p (make-list-1-a 14))
                              (cons 'q (make-list-1-a 14)))
                        (list (cons 'p (make-list-1-b 14))
                              (cons 'q (make-list-1-b 14)))
                        '()))
  #f)
(test-equal (not (unify (list (cons 'p (make-list-1-a 16))
                              (cons 'q (make-list-1-a 16)))
                        (list (cons 'p (make-list-1-b 16))
                              (cons 'q (make-list-1-b 16)))
                        '()))
  #f)
(test-equal (not (unify (list (cons 'p (make-list-1-a 18))
                              (cons 'q (make-list-1-a 18)))
                        (list (cons 'p (make-list-1-b 18))
                              (cons 'q (make-list-1-b 18)))
                        '()))
  #f)
(test-equal (not (unify (list (cons 'p (make-list-1-a 10))
                              (cons 'q (make-list-1-a 10)))
                        (list (cons 'p (make-list-1-b 10))
                              (cons 'q (make-list-1-b 10)))
                        '()))
  #f)
(test-end "unification exp-comp")
