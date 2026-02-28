#lang racket

(require racket/hash)

(define (no-merge x y hts)
  (list 'if '% (sub (car hts) x) (sub (cdr hts) y)))
  
(define (comp-atomic x y hts)
  (if (and (boolean? x) 
           (boolean? y))
    (comp-bool x y)
    (let ([x-sub (sub (car hts) x)]
          [y-sub (sub (cdr hts) y)])
     (if (equal? x-sub y-sub)
      x-sub
      (list 'if '% x-sub y-sub)))))

(define (comp-bool x y)
  (match* (x y)
    [(#t #t) #t]
    [(#f #f) #f]
    [(#t #f) '%]
    [(#f #t) '(not %)]))

(define (both x y z)
  (and (equal? x z) (equal? y z)))

(define (both-lambdas x y)
 (or (both (car x) (car y) 'lambda)
     (both (car x) (car y) 'λ)
     (and (equal? (car x) 'λ) (equal? (car y) 'lambda))
     (and (equal? (car x) 'lambda) (equal? (car y) 'λ))))

(define (extract-lmbd x y)
  (match* ((car x) (car y)) 
    [('lambda 'lambda) 'lambda]
    [('λ 'λ) 'λ]
    [(_ _) 'λ]))


(define (new-var-maps vars-x vars-y maps)
 (define merged-vars (for/list ([x vars-x] [y vars-y])
                               (if (equal? x y) x (string->symbol (format "~a!~a" x y)))))

 (define (make-pairs vars) (map cons vars merged-vars))
 (define map-x (hash-union (car maps) (make-immutable-hash (make-pairs vars-x)) #:combine (lambda (v1 v2) v2)))
 (define map-y (hash-union (cdr maps) (make-immutable-hash (make-pairs vars-y)) #:combine (lambda (v1 v2) v2)))
 (cons map-x map-y))

(define (sub ht expr)
  (if (list? expr)
    (map (lambda (e) (sub ht e)) expr)
    (hash-ref ht expr expr)))

(define (comp-lambda-expr x y hts)
  (let ([vars-x (cadr x)]
        [vars-y (cadr y)])
   (if (not (= (length vars-x) (length vars-y)))
     #| (no-merge x y) |#
     (comp-atomic x y hts)
     (let* ([lmbd (extract-lmbd x y)]
            [new-hts (new-var-maps vars-x vars-y hts)]
            [subbed-vars (sub (car new-hts) vars-x)]
            [body-x (caddr x)]
            [body-y (caddr y)])
       (list lmbd 
             subbed-vars 
             (comp-s-expr 
               body-x
               body-y
               new-hts))))))

(define (comp-ifs x y hts)
  (cons 'if (comp-lists (cdr x) (cdr y) hts)))

(define (comp-lists x y hts)
  (if (not(= (length x) (length y)))
    #| (no-merge x y) |#
    (comp-atomic x y hts)
    (cond
      ;empty lists
      [(= 0 (length x)) empty]

      ; lambdas
      ; TODO: maybe need to catch case where exactly one is a lambda expr
      ; like if case
      [(both-lambdas x y)
       (comp-lambda-expr x y hts)]

      ; if forms
      [(both (car x) (car y) 'if) (comp-ifs x y hts)]
      [(or (equal? (car x) 'if) (equal? (car y) 'if)) (comp-atomic x y hts)]

      ; quoted forms
      [(both (car x) (car y) 'quote) (comp-atomic x y hts)]

      ; function application
      [else (map (lambda (x y) (comp-s-expr x y hts)) x y)])))

(define (comp-s-expr x y hts)
   (cond
     [(and (list? x) (list? y)
           (comp-lists x y hts))]
     [else (comp-atomic x y hts)]))



(define (expr-compare x y)
  (comp-s-expr x y (cons (hash) (hash)))) 


(define (test-expr-compare x y)
  (and (equal? (eval x)
               (eval `(let ([% #t])
                           ,(expr-compare x y))))
                 
       (equal? (eval y)
               (eval `(let ([% #f])
                           ,(expr-compare x y))))))


(define test-expr-x '((lambda (a b) ((lambda (b a) (if (< a b) a b )) b (if (< 7 5) a (if (< 10 1) a b)))) 3 2))
(define test-expr-y '((lambda (b a) ((lambda (a b) (if (< a b) a b )) b a)) 3 7))
