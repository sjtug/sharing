#lang racket

; <prog> ::= <stmt> ...

; <stmt> ::= (label <label>)
;         |  (goto <label>)
;         |  (:= <var> <exp>)
;         |  (if <exp> goto <label>)

; <exp> ::= (+ <exp> <exp>)
;        |  (* <exp> <exp>)
;        |  (= <exp> <exp>)
;        |  <int>
;        |  <var>


; add a while construct to Racket:
(define-syntax while 
  (syntax-rules ()
    [(_ cond body ...)
     ;=>
     (letrec [(loop (λ ()
                      (when cond
                        body ...
                        (loop))))]
       (loop))]))


; stmt-map : label => stmt*
(define stmt-map (make-hasheq))

; constructs the label-to-statements map:
(define (preprocess stmts)
  (match stmts
    [(cons `(label ,label) rest)
     (hash-set! stmt-map label stmts)
     (preprocess rest)]
    
    [(cons _ rest)
     (preprocess rest)]
    
    ['()
     (void)]))


; states pair statements with environments:
(struct state {stmts env})

; an environment maps variables to integers:
(define env0 (make-immutable-hasheq))

; inject creates an initial state for a program:
(define (inject prog)
  (state prog env0))

; exp-eval : exp env -> integer
(define (exp-eval exp env)
  (match exp
    [(? symbol?)
     ;=>
     (hash-ref env exp)]
    
    [(? integer?)
     ;=>
     exp]
    
    [`(+ ,exp1 ,exp2)
     ;=>
     (+ (exp-eval exp1 env)
        (exp-eval exp2 env))]
    
    [`(* ,exp1 ,exp2)
     ;=>
     (* (exp-eval exp1 env)
        (exp-eval exp2 env))]
    
    [`(= ,exp1 ,exp2)
     ;=>
     (if (= (exp-eval exp1 env)
            (exp-eval exp2 env))
         1
         0)]))

; step : state -> state
(define (step state0)
  
  (define stmts (state-stmts state0))
  
  (define env (state-env state0))
  
  (match stmts
    ['()
     ;=>
     env]
    
    [(cons `(label ,l) rest)
     ;=>
     (state rest env)]
    
    [(cons `(:= ,var ,exp) rest)
     ;=>
     (define env* (hash-set env var (exp-eval exp env)))
     (state rest env*)]
    
    [(cons `(if ,exp goto ,label) rest)
     ;=>
     (define condition (exp-eval exp env))
     (if (not (= condition 0))
         (state (hash-ref stmt-map label) env)
         (state rest env))]
    
    [(cons `(goto ,label) rest)
     ;=>
     (state (hash-ref stmt-map label) env)]
         
    [else
     ;=>
     (error (format "unknown instruction: ~a!" (car stmts)))]))

; run runs a program to termination:
(define (run prog)
  (preprocess prog)
  (define current-state (inject prog))
  (while (state? current-state)
    (set! current-state (step current-state))))

(define prog 
  '(
    
    (:= n 5)
    
    (:= a 1)
    
    (label top)
    
    (if (= n 0) goto done)
    
    (:= a (* a n))
    
    (:= n (+ n -1))
    
    (goto top)
    
    (label done)
    
    ))


(run prog)