(define-module (minikanren kanren)
  #:use-module (minikanren streams)
  #:export (initial-kanren
	    counter substitution disequality-store
	    modified-counter
	    modified-substitution
	    modified-disequality-store
	    call/fresh)
  #:re-export (conj disj conj/dfs disj/dfs))
(use-modules (srfi srfi-9))
(use-modules (minikanren unification))

;;
;; The kanren structure that holds state
;;

(define-record-type <kanren>
  (make-kanren c s d)
  kanren?
  (c counter)
  (s substitution)
  (d disequality-store))

(define initial-kanren
  (make-kanren 0 '() '()))

;; Using these modified-* functions instead of make-kanren
;; make-kanren let us add new fields without having to
;; change existing code

(define (modified-counter f k)
  (make-kanren (f (counter k))
               (substitution k)
               (disequality-store k)))

(define (modified-substitution f k)
  (make-kanren (counter k)
               (f (substitution k))
               (disequality-store k)))

(define (modified-disequality-store f k)
  (make-kanren (counter k)
               (substitution k)
               (f (disequality-store k))))

(define (1+ n) (+ 1 n))

(define (call/fresh f)
  (lambda (k)
    ((f (var (counter k))) (modified-counter 1+ k))))
