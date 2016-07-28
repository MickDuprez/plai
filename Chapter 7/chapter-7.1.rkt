#lang plai-typed
;; chapter 7.1 solution


;; core functions (low level)
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])


(define-type ExprS
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : ExprS)]
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)])

;; core language functions:
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "one or more arguments was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "one or more arguments was not a number")]))

;; parser
;; parser: s-expression -> ExprS
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

;; Binding
(define-type Binding
  [bind (name : symbol) (val : Value)])

;; some helper functions:
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;; helper function that retrieves a binding value from the Env:
;; lookup: symbol * env -> number
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "reference to undefined symbol")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;; define a Value type to be used as a return value from interp:
(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

;; the interpreter:
;; interp : EprC * Env -> Value
(define (interp [e : ExprC] [env : Env] ) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a)
          (local ([define fd (interp f env)])
            (interp (funV-body fd)
                    (extend-env (bind (funV-arg fd)
                                      (interp a env))
                                env)))]
    [fdC (n a b) (funV n a b)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    ))



(display "\n**** env interp tests ****\n")
(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env) (numV 7))