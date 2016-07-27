#lang plai-typed
;; chapter 6 solution


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

;; Binding
(define-type Binding
  [bind (name : symbol) (val : number)])

;; some helper functions:
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;; helper function that retrieves a binding value from the Env:
;; lookup: symbol * env -> number
(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "reference to undefined symbol")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;; the interpreter:
;; interp : EprC * listof fds * Env -> number
(define (interp [e : ExprC] [env : Env] [fds : (listof FuncDefC)] ) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (x) (lookup x env)]
    [appC (f a)
          (local ([define fd (get-fundef f fds)]) ; create a local func from our fds
            (interp (fdC-body fd)
                    (extend-env (bind (fdC-arg fd)(interp a env fds))
                                mt-env) fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    ))

;; tests for general arithmetic:
(display "\n**** general arithmetic tests ****\n")
(test (interp (desugar (parse '(+ 1 2))) mt-env fds) 3)
(test (interp (desugar (parse '(+ 1 (* 2 2)))) mt-env fds) 5)
(test (interp (desugar (parse '(+ (* 1 2) (+ 1 2)))) mt-env fds) 5)
(test (interp (desugar (parse '(- (* 1 2) (+ 1 2)))) mt-env fds) -1)
(test (interp (desugar (parse '(- 15 7))) mt-env fds) 8)
(test (interp (desugar (parse '(neg 5))) mt-env fds) -5)
(test (interp (desugar (parse '(neg -5))) mt-env fds) 5)

;; tests for pasing functions:
(display "\n**** general arithmetic tests ****\n")
(test (interp (desugar (parse '(call double 5))) mt-env fds) 10)
(test (interp (desugar (parse '(call quadruple 5))) mt-env fds) 20)

(display "\n**** env interp tests ****\n")
(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)

(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

(display "\n**** faulty test ****\n")
(interp (appC 'f1 (numC 3))
        mt-env
        (list (fdC 'f1 'x (appC 'f2 (numC 4)))
              (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
