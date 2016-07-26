#lang plai-typed
;; chapter 5 solution


;; core functions (low level)
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

;; changes from ArithS to allow more general expressions.
(define-type ExprS
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : ExprS)]
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)])

;; defining a Function defintion type:
(define-type FuncDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

;; subtitutes the parsed values for the symbols in the function bodies.
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))

;; some more complex function definitions:
(define double (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(define quadruple (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))
(define const5 (fdC 'const5 '_ (numC 5)))

;; define our list of fundef's
(define fds (list double quadruple const5))

;; parser
(define (parse [s : s-expression]) : ExprS
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
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)) )]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;; desugar method to convert surface level functions to use/build upon core functions
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [idS (s) (idC s)]
    [appS (f a) (appC f (desugar a))]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (desugar (bminusS (numS 0)  e))]
    ))

;; helper function that returns a function from the function library list by name.
(define (get-fundef [n : symbol] [fds : (listof FuncDefC)]) : FuncDefC
                    (cond
                      [(empty? fds) (error 'get-fundef "reference to undefined function")]
                      [(cons? fds) (cond
                                     [(equal? n (fdC-name (first fds))) (first fds)]
                                     [else (get-fundef n (rest fds))])]))
                      

;; the interpreter:
(define (interp [e : ExprC] [fds : (listof FuncDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "should not get here - (idC)")]
    [appC (f a) (local ([define fd (get-fundef f fds)]) ; create a local func from our fds
                  (interp (subst a                      ; substitute 'a' (the passed variable)
                                 (fdC-arg fd)           ; for the arg of fd
                                 (fdC-body fd))         ; in the local func def fd
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    ))

;; tests for general arithmetic:
(display "\n**** general arithmetic tests ****\n")
(test (eq? (interp (desugar (parse '(+ 1 2))) fds) 3) #t)
(test (eq? (interp (desugar (parse '(+ 1 (* 2 2))))fds) 5) #t)
(test (eq? (interp (desugar (parse '(+ (* 1 2) (+ 1 2))))fds) 5) #t)
(test (eq? (interp (desugar (parse '(- (* 1 2) (+ 1 2))))fds) -1) #t)
(test (eq? (interp (desugar (parse '(- 15 7)))fds) 8) #t)
(test (eq? (interp (desugar (parse '(neg 5)))fds) -5) #t)
(test (eq? (interp (desugar (parse '(neg -5)))fds) 5) #t)

;; tests for pasing functions:
(display "\n**** general arithmetic tests ****\n")
(test (eq? (interp (desugar (parse '(call double 5)))fds) 10) #t)
(test (eq? (interp (desugar (parse '(call quadruple 5)))fds) 20) #t)
