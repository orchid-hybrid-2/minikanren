(use-modules (minikanren language)
	     (minikanren examples goat))
(use-modules (srfi srfi-64))

(test-begin "goat")
(test-equal (length (run* goat))
  2)
(test-end "goat")
