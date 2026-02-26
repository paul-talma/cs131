#lang racket

(define (fact n)
  (cond
    [(eq? n 0) 1]
    [else (* n (fact (- n 1)))]))
  

(define (fact_ n)
  (define (helper n acc)
    (cond
      [(eq? n 0) acc]
      [else (helper (- n 1) (* acc n))]))
  (helper n 1))

(define (flatten ls)
  (define (dfs ls acc)
    (cond
      [(null? ls)  acc]
      [(pair? (car ls))  (dfs (cdr ls) (dfs (car ls) acc))]
      [else (dfs (cdr ls) (cons (car ls) acc))]))
    
  (reverse (dfs ls empty)))


(define (cxr instr)
   (cond
     [(empty? instr) (lambda (x) x)]
     [(not (member (car instr) '(a d))) (error "invalid input")]
     [(equal? (car instr) 'a) (lambda (ls) ((cxr (cdr instr)) (car ls)))]
     [(equal? (car instr) 'd) (lambda (ls) ((cxr (cdr instr)) (cdr ls)))]))
    
(flatten '(1 (2 (3 4)) 5))
