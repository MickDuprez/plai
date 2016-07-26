#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)])

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (desugar(bminusS (numS 0)  e))]
    ))

(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl))
                              (parse (third sl)))]
         [(*) (multS (parse (second sl))
                              (parse (third sl)))]
         [(-) (bminusS (parse (second sl))
                                (parse (third sl)))]
         [(neg) (uminusS (parse (second sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    ))


(test (eq? (interp (desugar (parse '(+ 1 2)))) 3) #t)
(test (eq? (interp (desugar (parse '(+ 1 (* 2 2))))) 5) #t)
(test (eq? (interp (desugar (parse '(+ (* 1 2) (+ 1 2))))) 5) #t)
(test (eq? (interp (desugar (parse '(- (* 1 2) (+ 1 2))))) -1) #t)
(test (eq? (interp (desugar (parse '(- 15 7)))) 8) #t)
(test (eq? (interp (desugar (parse '(neg 5)))) -5) #t)
(test (eq? (interp (desugar (parse '(neg -5)))) 5) #t)
