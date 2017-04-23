(use-modules (minikanren language)
	     (minikanren examples four-color))

(define (timing-test thk)
  (let ((t1 (current-time)))
    (thk)
    (- (current-time) t1)))

(display (timing-test (lambda ()
			(display "Australia: ")
			(display (do-australia)))))
(newline)

(display (timing-test (lambda ()
			(display "Canada: ")
			(display (do-canada)))))
(newline)

(display (timing-test (lambda ()
			(display "America: ")
			(display (do-america)))))
(newline)
