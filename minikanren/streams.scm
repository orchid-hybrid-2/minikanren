(define-module (minikanren streams)
  #:export (pull stream-map stream-take take take-all
		 mzero mplus mplus/dfs unit mbind mbind/dfs mapm
		 conj disj conj/dfs disj/dfs))

;; A stream is one of
;;  * nil
;;  * a pair whose cdr is a stream
;;  * a zero arg lambda 'delaying' a pair or nil
;;    (can't delay twice!)

(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (stream-map f s)
  (cond ((null? s) s)
	((pair? s) (cons (f (car s)) (stream-map f (cdr s))))
	((procedure? s)
	 (lambda ()
	   (stream-map f (s))))))

(define (stream-take n s)
  (if (= n 0)
      '()
      (cond ((null? s) s)
	    ((pair? s) (cons (car s) (stream-take (- n 1) (cdr s))))
	    ((procedure? s)
	     (lambda ()
	       (stream-take n (s)))))))

(define (take n $)
  (if (zero? n)
      '()
      (let (($ (pull $)))
	(if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $)
        '()
        (cons (car $) (take-all (cdr $))))))

;;
;; The monad for fair search that carries the kanren around
;;

(define mzero '())

(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) ;; Switch for fairness, give every branch a chance
    (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (mplus/dfs $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) ;; Don't switch!
    (lambda () (mplus/dfs ($1) $2)))
   (else (cons (car $1) (mplus/dfs (cdr $1) $2)))))


(define (unit k) (cons k mzero))

(define (mbind $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (mbind ($) g)))
   (else (mplus (g (car $)) (mbind (cdr $) g)))))

(define (mbind/dfs $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (mbind/dfs ($) g)))
   (else (mplus/dfs (g (car $)) (mbind/dfs (cdr $) g)))))


(define (disj g1 g2) (lambda (k) (mplus (g1 k) (g2 k))))
(define (conj g1 g2) (lambda (k) (mbind (g1 k) g2)))

(define (disj/dfs g1 g2) (lambda (k) (mplus/dfs (g1 k) (g2 k))))
(define (conj/dfs g1 g2) (lambda (k) (mbind/dfs (g1 k) g2)))


(define (mapm f l)
  (if (null? l)
      (unit '())
      (mbind (f (car l))
	     (lambda (v)
	       (mbind (mapm f (cdr l))
		      (lambda (vs)
			(unit (cons v vs))))))))
