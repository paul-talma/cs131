#lang racket

(define (comp-bool x y)
  (match* (x y)
    [(#t #t) #t]
    [(#f #f) #f]
    [(#t #f) '%]
    [(#f #t) '(not %)]))
    
  

(define (comp-literal x y)
  (if (equal? x y)
    x
    (list 'if '% x y)))

#| (define (comp-lists x y) |#
    #|   (cond  |#
          #|         [(= (length x) (length y)) |#
                      #|          (cond |#
                                   #|            [((length x) 0) empty] |#
                                   #|            #| [(lambda? (car x))] |# |#
                                   #|            [else (map (expr-compare x y))])] |#
          #|         [else `(if % ,x ,y)])) |#
  
(define (comp-lists x y)
  (map (expr-compare x y)))

(define (expr-compare x y)
  (cond
   [(and (boolean? x) (boolean? y)) 
    (comp-bool x y)]
   [(and (number? x) (number? y)) 
    (comp-literal x y)]
   [(and (string? x) (string? y)) 
    (comp-literal x y)]
   [(and (symbol? x) (symbol? y)) 
    (comp-literal x y)]
   [(and (list? x) (list? y))
    (comp-lists x y)]
   [else (list 'if '% x y)]))
    
     


(expr-compare 12 12)
(expr-compare 12 20)
(expr-compare #t #t)
(expr-compare #f #f) 
(expr-compare #t #f)
(expr-compare #f #t)
(expr-compare "hello" "world")
(expr-compare '(list 1 2 3) '(list 1 2 3))
(expr-compare '(list 1 2 3) '(list 1 2 4))
(expr-compare '(cons a b) '(cons a b))
(expr-compare '(cons a b) '(cons a c))
;(expr-compare '(1 2 3) '(1 2 4))
;expr-compare '(1 2 3) '(1 2))
;expr-compare '(list 1 (list 2 3) 4) '(list 1 2 4))

#| (expr-compare '(/ 1 0) '(/ 1 0.0)) |#
