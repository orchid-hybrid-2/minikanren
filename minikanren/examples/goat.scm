(define-module (minikanren examples goat)
  #:export (goat))
(use-modules (minikanren language)
	     (minikanren examples lists))

;; This is a port of the solution by Mihaela Malita
;; <http://www.anselm.edu/internet/compsci/faculty_staff/mmalita/HOMEPAGE/logic/goat.txt>

;; A farmer has to cross a river with a wolf, a goat and a cabbage.
;; He has a boat, but in the boat he can take just one thing.
;; He cannot let the goat alone with the wolf or the goat with the cabbage. It's obvious why.

(define (opp x y)
  ;; We call the start side of the river 's
  ;; the other side of the river (the goal) 'g
  (conde ((== x 's) (== y 'g))
	 ((== x 'g) (== y 's))))

(define (acceptable-state s)
  ;; A test that ensures the cabbage and goat are safe
  (fresh (t g c w)
    (== s `(state (farmer ,t) (goat ,g) (cabbage ,c) (wolf ,w)))
    (conde ((=/= g w) (=/= g c))
	   ((== g w) (=/= g c) (== t g))
	   ((=/= g w) (== g c) (== t g))
	   ((== g w) (== g c) (== t g)))))

(define (from s1 s2)
  (fresh (t1 g1 c1 w1
	     t2 g2 c2 w2)
    (conde (;; Farmer takes goat
	    (== s1 `(state (farmer ,t1) (goat ,t1) (cabbage ,c1) (wolf ,w1)))
	    (== s2 `(state (farmer ,t2) (goat ,t2) (cabbage ,c1) (wolf ,w1)))
	    (opp t1 t2))
	   (;; Farmer takes cabbage
	    (== s1 `(state (farmer ,t1) (goat ,g1) (cabbage ,t1) (wolf ,w1)))
	    (== s2 `(state (farmer ,t2) (goat ,g1) (cabbage ,t2) (wolf ,w1)))
	    (opp t1 t2))
	   (;; Farmer takes wolf
	    (== s1 `(state (farmer ,t1) (goat ,g1) (cabbage ,c1) (wolf ,t1)))
	    (== s2 `(state (farmer ,t2) (goat ,g1) (cabbage ,c1) (wolf ,t2)))
	    (opp t1 t2))
	   (;; Farmer goes alone
	    (== s1 `(state (farmer ,t1) (goat ,g1) (cabbage ,c1) (wolf ,w1)))
	    (== s2 `(state (farmer ,t2) (goat ,g1) (cabbage ,c1) (wolf ,w1)))
	    (opp t1 t2)))))

(define (goat sol)
  (bkt '(state (farmer s) (goat s) (cabbage s) (wolf s))
       '()
       sol))

(define (bkt state path sol)
  (conde ((== state '(state (farmer g) (goat g) (cabbage g) (wolf g)))
	  (== sol `(,state . ,path)))
	 ((fresh (n1)
	    (from state n1)
	    (acceptable-state n1)
	    (not-membero n1 path)
	    (bkt n1 (cons state path) sol)))))

;; > (run* goat)
;; $2 = ((((state (farmer g) (goat g) (cabbage g) (wolf g))
;;         (state (farmer s) (goat s) (cabbage g) (wolf g))
;;         (state (farmer g) (goat s) (cabbage g) (wolf g))
;;         (state (farmer s) (goat s) (cabbage s) (wolf g))
;;         (state (farmer g) (goat g) (cabbage s) (wolf g))
;;         (state (farmer s) (goat g) (cabbage s) (wolf s))
;;         (state (farmer g) (goat g) (cabbage s) (wolf s))
;;         (state (farmer s) (goat s) (cabbage s) (wolf s))) where)
;;       (((state (farmer g) (goat g) (cabbage g) (wolf g))
;;         (state (farmer s) (goat s) (cabbage g) (wolf g))
;;         (state (farmer g) (goat s) (cabbage g) (wolf g))
;;         (state (farmer s) (goat s) (cabbage g) (wolf s))
;;         (state (farmer g) (goat g) (cabbage g) (wolf s))
;;         (state (farmer s) (goat g) (cabbage s) (wolf s))
;;         (state (farmer g) (goat g) (cabbage s) (wolf s))
;;         (state (farmer s) (goat s) (cabbage s) (wolf s))) where))
