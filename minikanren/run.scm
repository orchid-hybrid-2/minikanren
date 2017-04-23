(define-module (minikanren run)
  #:export (run^ run* runi))
(use-modules (minikanren binary-trie)
	     (minikanren streams)
	     (minikanren unification)
	     (minikanren kanren))

(define (print s) (display s) (newline))

;;
;; Reifying terms and kanren states
;;

(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(define (reify-s v s)
  ;; Given a term and substitution this extends
  ;; the substitution with nice names for each
  ;; fresh variable in v
  (let ((v (walk v s)))
    (cond
     ((var? v) (let ((n (reify-name (trie-size s))))
                 (trie-insert s (var->int v) n)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-term t s)
  ;; To reify a term:
  ;;
  ;; First use walk* so that we have a flat term
  ;; containing only fresh variables
  ;;
  ;; Then extend our substitution with good names
  ;; for all those variables
  ;;
  ;; walk* it again to reify all the fresh variables
  ;; in the term itself.
  ;;
  ;; Doing it in two steps like this means we use up
  ;; names for any variables that aren't in the final
  ;; term
  (let ((v (walk* t s)))
    (walk* v (reify-s v '()))))

(define (reify-kanren k)
  ;; The query variable will always be the very first
  ;; one so reify (var 0) along with all the constraints
  ;; or extra conditions that might need to be displayed
  (define (make-=/= eq) `(=/= ,(car eq) ,(cdr eq)))
  (reify-term `(,(var 0) where .
                ,(append (map (lambda (d)
                                `(or . ,(map make-=/= d)))
                              (disequality-store k))))
              (substitution k)))

;;
;; Running a goal
;;

(define (run^ n g)
  ;; Compute up to a set limit of results
  (map reify-kanren (take n ((call/fresh g) initial-kanren))))

(define (run* g)
  ;; Compute every result
  (map reify-kanren (take-all ((call/fresh g) initial-kanren))))

(define (runi g)
  ;; This version of run returns one result from the
  ;; stream at a time interactively asking if you
  ;; want more or not
  (let (($ ((call/fresh g) initial-kanren)))
    (let loop (($ (pull $)))
      (if (null? $)
          (print 'thats-all!)
          (begin (print (reify-kanren (car $)))
                 (print '(another? y/n))
                 (case (read)
                   ((y yes) (loop (pull (cdr $))))
                   (else (print 'bye!))))))))
