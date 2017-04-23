(use-modules (minikanren binary-trie))
(use-modules (minikanren unification))
(use-modules (srfi srfi-64))

(let ((u (var 0)) (v (var 1)))
  (test-begin "unification atoms")
  (test-equal (unify 'x 'x '())
    '())
  (test-equal (unify 'x 'y '())
    #f)
  (test-end "unification atoms")
  
  (test-begin "unification structures")
  (test-equal (unify '(x y) '(x y) '())
    '())
  (test-equal (unify '(x y) '(- y) '())
    #f)
  (test-equal (unify '(x y) '(x -) '())
    #f)
  (test-end "unification structures")

  (test-begin "unification variable")
  (test-equal (walk* u (unify 'x u '()))
    'x)
  (test-equal (walk* u (unify u 'x '()))
    'x)
  (test-end "unification variable")

  (test-begin "unification two variables")
  (let ((s (unify u v '())))
    (test-equal (walk* u s) (walk* v s)))
  
  (let ((s (unify v u '())))
    (test-equal (walk* u s) (walk* v s)))
  
  (let ((s (unify (list u v) (list 'x 'y) '())))
    (test-equal '(x y) (walk* (list u v) s)))
  
  (let ((s (unify (list u 'y) (list 'x v) '())))
    (test-equal '(x y) (walk* (list u v) s)))

  (let ((s (unify (list u 'y) (list 'x v) '())))
    (test-equal '(x y) (walk* (list u v) s)))

  (test-equal (unify (list u 'x 'y) (list v u v) '())
    #f)
  (test-end "unification two variables")

  (test-begin "unification occurs check")
  (test-equal (unify u (list u) '())
    #f)
  (test-equal (unify (list u v) (list v (list u)) '())
    #f)
  (test-end "unification occurs check"))
