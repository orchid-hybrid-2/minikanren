(use-modules (minikanren language)
	     (minikanren dcg))
(use-modules (srfi srfi-64))

;; Prolog and Natural-Language Analysis book DCG example 3.11

(--> (s x)
  (fresh (np- vp-)
     (conde ((== x `(s ,np- ,vp-)) (np np-) (vp vp-)))))

(--> (np x)
 (fresh (det- n- rel- np- pn-)
   (conde
     ((== x `(np ,det- ,n- ,rel-))
      (det det-) (n n-) (optrel rel-))
     ((== x `(np ,pn-)) (pn pn-)))))

(--> (vp x) 
     (fresh (tv- np- iv-)
       (conde ((== x `(vp ,tv- ,np-)) (tv tv-) (np np-))
              ((== x `(vp ,iv-)) (iv iv-)))))

(--> (optrel x)
     (fresh (vp-)
      (conde
       ((== x `(rel epsilon)) '())
       ((== x `(rel that ,vp-)) '(that) (vp vp-)))))

(--> (pn x)
     (conde
      ((== x `(pn terry)) '(terry))
      ((== x `(pn shrdlu)) '(shrdlu))))
 
(--> (iv x)
     (conde ((== x `(iv halts)) '(halts))))
 
(--> (det x)
     (conde ((== x `(det a)) '(a))))
 
(--> (n x)
     (conde ((== x `(n program)) '(program))))
 
(--> (tv x)
     (conde ((== x `(tv writes)) '(writes))))


(test-begin "dcg")
(test-equal (run* (lambda (q)
                    (s q '(terry writes a program that writes terry) '())))
  '(((s (np (pn terry))
        (vp (tv writes)
            (np (det a)
                (n program)
                (rel that (vp (tv writes)
                              (np (pn terry))))))) where)))
(test-end "dcg")
