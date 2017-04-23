(define-module (minikanren language)
  #:use-module (minikanren run)
  #:use-module (minikanren disequality)
  #:export (fresh conde fresh/dfs conde/dfs ==)
  #:re-export (run^ run* runi =/=))
(use-modules (minikanren streams)
	     (minikanren kanren)
	     (minikanren unification))
(use-modules (srfi srfi-11))

;;
;; MiniKanren
;;

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (k) (lambda () (g k))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

;; depth first search versions of the same

(define-syntax conj+/dfs
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj/dfs g0 (conj+/dfs g ...)))))

(define-syntax disj+/dfs
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj/dfs g0 (disj+/dfs g ...)))))

(define-syntax fresh/dfs
  (syntax-rules ()
    ((_ () g0 g ...) (conj+/dfs g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh/dfs (x ...) g0 g ...))))))

(define-syntax conde/dfs
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+/dfs (conj+ g0 g ...) ...))))

;;
;; Unification
;;

(define (== u v)
  (lambda (k)
    (let-values (((s p) (unify/prefix u v (substitution k))))
      (if s
	  (normalize-disequality-store
           (modified-substitution (lambda (_) s) k))
	  mzero))))
