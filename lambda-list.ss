
(define (make-checks id check*)
  (map (lambda (chk)
         #`(unless (#,chk #,id)
             (error #f "value does not satisfy check" '#,id #,id '#,chk)))
    check*))

(define (position-ok pre-opt? arg-list a)
  (unless pre-opt?
    (syntax-violation 'lambda-list "required argument after optional" arg-list a)))

(define (parse-args args)
  (let loop ([arg* args] [pre-opt? #t] [req #'()] [opt #'()] [def #'()] [checks #'()])
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
  (with-syntax
    ([(ids ...) (map (lambda (a)
                       (syntax-case a ()
                         [(id _ ...) #'id]
                         [id #'id]))
                  args)])
    #'(ids ...)))

(define (find-duplicates ls)
  (let loop ([set (make-enumeration '())] [items ls] [dupes '()])
    (if (null? items)
        (if (null? dupes) #f dupes)
        (let ([h (car items)] [t (cdr items)])
          (if (enum-set-member? h set)
              (loop set (cdr items) (cons h dupes))
              (loop (make-enumeration (cons h (enum-set->list set)))
                (cdr items) dupes))))))

(define (syntax-cdr x)
  (syntax-case x ()
    [(i * ...) #'(* ...)]))

(define (build-lambda arg-list rest body)
  (let* ([ids (extract-arg-ids arg-list)]
         [dupes (find-duplicates (map syntax->datum ids))])
    (for-each (lambda (id)
                (unless (identifier? id)
                  (syntax-violation 'lambda-list "Invalid identifier" x id)))
      ids)
    (when dupes
      (syntax-violation 'lambda-list "Duplicate identifiers in lambda-list" x dupes))
    (let-values ([(req opt def checks) (parse-args arg-list)]
                 #;[(rest-req rest-opt rest-def rest-check) (parse-args rest)])
      (if (null? (syntax->list opt))
          #`(lambda #,req #,@checks #,@body)
          #`(letrec
              ([self
                (case-lambda
                  #,@(let loop ([given req] [absent opt] [default def])
                       (syntax-case absent ()
                         [() #`((#,given #,@checks #,@body))]
                         [(a abs* ...)
                          #`((#,given (self #,@given #,@default))
                             #,@(loop #`(#,@given a) #'(abs* ...) (syntax-cdr default)))])))])
              self)))))

(define-syntax lambda-list
  (lambda (x)
    (syntax-case x (default: satisfies: rest:)
      [(_ a* b b* ...) (identifier? #'a*)
       #'(lambda a* b b* ...)]
      [(_ (a* ... (rest: a other ...)) b b* ...)
       (build-lambda #'(a* ...) #'(a other ...) #'(b b* ...))]
      [(_ (a* ...) b b* ...)
       (build-lambda #'(a* ...) #f #'(b b* ...))])))
