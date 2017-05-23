
(define (make-checks id check*)
  (map (lambda (chk)
         #`(unless (#,chk #,id)
             (error #f "value does not satisfy check" '#,id #,id '#,chk)))
    check*))

(define (position-ok pre-opt? arg-list a)
  (unless pre-opt?
    (syntax-violation 'lambda-list "required argument after optional" arg-list a)))

(define (parse-args args)
  (let loop ([arg* args] [pre-opt? #t] [req '()] [opt '()] [def '()] [checks '()])
    (if (null? arg*)
        (values req opt def checks)
        (syntax-case (car arg*) (default: satisfies:)
          [(a default: val) (identifier? #'a)
           (loop (cdr arg*) #f req #`(#,@opt a) #`(#,@def val) checks)]
          [(a satisfies: (check?* ...))
           (begin
             (position-ok pre-opt? args #'a)
             (loop (cdr arg*) #t #`(#,@req a) opt def #`(#,@checks #,@(make-checks #'a #'(check?* ...)))))]
          [(a default: val satisfies: (check?* ...))
           (loop (cdr arg*) #f req #`(#,@opt a) #`(#,@def val) #`(#,@checks #,@(make-checks #'a #'(check?* ...))))]
          [(a satisfies: (check?* ...) default: val)
           (loop (cdr arg*) #f req #`(#,@opt a) #`(#,@def val) #`(#,@checks #,@(make-checks #'a #'(check?* ...))))]
          [a (identifier? #'a)
           (begin
             (position-ok pre-opt? args #'a)
             (loop (cdr arg*) #t #`(#,@req a) opt def checks))]
          [_ (syntax-violation 'lambda-list "Improper argument list" args arg*)]))))

(define (extract-arg-ids args)
  (map (lambda (a)
         (syntax-case a ()
           [(id _ ...) #'id]
           [id #'id]))
    args))

(define (find-duplicates ls)
  (let loop ([set (make-enumeration '())] [items ls] [dupes '()])
    (if (null? items)
        (if (null? dupes) #f dupes)
        (let ([h (car items)] [t (cdr items)])
          (if (enum-set-member? h set)
              (loop set (cdr items) (cons h dupes))
              (loop (make-enumeration (cons h (enum-set->list set)))
                (cdr items) dupes))))))

(define (build-lambda arg-list rest body)
  (let* ([ids (extract-arg-ids (if rest
                                   (cons rest arg-list)
                                   arg-list))]
         [dupes (find-duplicates (map syntax->datum ids))])
    (for-each (lambda (id)
                (unless (identifier? id)
                  (syntax-violation 'lambda-list "Invalid identifier" #`(#,@arg-list . #,rest) id)))
      ids)
    (when dupes
      (syntax-violation 'lambda-list "Duplicate identifiers in lambda-list" #`(#,@arg-list . #,rest) dupes))
    (let-values ([(req opt def checks) (parse-args arg-list)])
      (if (null? opt)
          (if rest
              #`(lambda (#,@req . #,rest) #,@checks #,@body)
              #`(lambda #,req #,@checks #,@body))
          #`(letrec
              ([self
                #,(if rest
                      #`(lambda (#,@req #,@opt . #,rest)
                          #,@checks #,@body)
                      #`(lambda (#,@req #,@opt)
                          #,@checks #,@body))])
              (case-lambda
                #,@(let loop ([given req] [absent opt] [default def])
                     (if (null? absent)
                         (if rest
                             #`(((#,@given . #,rest) (apply self #,@given #,rest)))
                             #`((#,given (self #,@given))))
                         #`((#,given (self #,@given #,@default))
                            #,@(loop #`(#,@given #,(car absent)) (cdr absent) (cdr default)))))))))))

(define-syntax lambda-list
  (lambda (x)
    (syntax-case x (default: satisfies:)
      [(_ a* b b* ...) (identifier? #'a*)
       #'(lambda a* b b* ...)]
      [(_ (a* ...) b b* ...)
       (build-lambda #'(a* ...) #f #'(b b* ...))]
      [(_ (a* ... . rest) b b* ...) (identifier? #'rest)
       (build-lambda #'(a* ...) #'rest #'(b b* ...))])))
