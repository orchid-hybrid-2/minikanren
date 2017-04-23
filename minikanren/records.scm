(define-module (minikanren records)
  #:export (define-record record? explode-record apply-record-constructor))

;; This code is extremely specific to guiles low level structures library
;; The purpose of it is to provide a way to create new disjoint data types
;; that may be used in minikanren programming and do not unify with any
;; existing objects.

(define (iota i n)
  (if (= i n)
      '()
      (cons i (iota (+ i 1) n))))

(define (make-vtable-spec number-of-fields)
  (list->string (apply append (make-list (+ 1 number-of-fields) '(#\p #\r)))))

(define (make-format-string structure-name fields)
  (string-append "#<" structure-name
                 (apply string-append
                        (map (lambda (field)
                               (string-append " " (symbol->string field) ":~a"))
                             fields))
                 ">"))

(define-syntax define-record
  (lambda (x)
    (syntax-case x ()
      ((define-record <name> (<field-name> <field-accessor>) ...)
       (let* ((structure-name (symbol->string (syntax->datum #'<name>)))
              (fields (syntax->datum #'(<field-name> ...)))
              (accessors (syntax->datum #'(<field-accessor> ...)))
              (number-of-fields (length fields))
              (field-indices (iota 0 number-of-fields)))
         (with-syntax ((rrr (datum->syntax x (make-vtable-spec number-of-fields)))
                       (format-string (datum->syntax x (make-format-string structure-name fields))))
           #`(begin
               (define vtable
                 (make-vtable rrr
                              (lambda (struct port)
                                (format port format-string
                                        #,@(map (lambda (i)
                                                  #`(struct-ref struct #,(datum->syntax x i)))
                                                field-indices)))))
               ;;(set-struct-vtable-name! vtable '<name>)
               (define (<name> <field-name> ...)
                 (make-struct/no-tail vtable '<name> <field-name> ...))
               #,@(map (lambda (accessor field-index)
                         #`(define (#,(datum->syntax x accessor) struct)
                             (struct-ref struct #,(datum->syntax x (+ 1 field-index)))))
                       accessors
                       field-indices))))))))

(define (record? s)
  (if (struct? s)
      ;;(struct-vtable-name (struct-vtable s))
      (struct-ref s 0)
      #f))

(define (explode-record s)
  (let loop ((i 0) (l (string->list (symbol->string (struct-ref (struct-vtable s) vtable-index-layout)))))
    (if (null? l)
        '()
        (cons (struct-ref s i)
              (loop (+ i 1) (cddr l))))))

(define (apply-record-constructor s args)
  (apply make-struct/no-tail (struct-vtable s) (struct-ref s 0) args))
