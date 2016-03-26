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




;;;;; Abstract


; states pair statements with environments:
(struct astate {stmts aenv}
  #:transparent)

; an environment maps variables to integers:
(define aenv0 (make-immutable-hasheq))

; inject creates an initial state for a program:
(define (ainject prog)
  (astate prog aenv0))

; alpha : integer -> abstract-integer
(define (alpha n)
  (cond
    [(< n 0)  {set '-}]
    [(= n 0)  {set 0}]
    [(> n 0)  {set '+}]))

; +/abstract : abstract-integer abstract-integer -> abstract-integer
(define (+/abstract an1 an2)
  (apply set-union
         (for*/list ([s1 an1]
                    [s2 an2])
           (+/alpha s1 s2))))

; +/alpha : sign sign -> abstract-integer
(define (+/alpha s1 s2)
  (match* (s1 s2)
    [('- '-)  {set '-}]
    [('-  0)  {set '-}]
    [('- '+)  {set '- 0 '+}]
    [('0  x)  {set x}]
    [('+ '-)  {set '- 0 '+}]
    [('+  0)  {set '+}]
    [('+ '+)  {set '+}]))

; */abstract : abstract-integer abstract-integer -> abstract-integer
(define (*/abstract an1 an2)
  (apply set-union
         (for*/list ([s1 an1]
                    [s2 an2])
           (*/alpha s1 s2))))

; */alpha : sign sign -> abstract-integer
(define (*/alpha s1 s2)
  (match* (s1 s2)
    [('- '-)  {set '+}]
    [('-  0)  {set  0}]
    [('- '+)  {set '-}]
    [('0  x)  {set  0}]
    [('+ '-)  {set '-}]
    [('+  0)  {set  0}]
    [('+ '+)  {set '+}]))
   

; exp-aeval : exp aenv -> abstract-integer
(define (exp-aeval exp aenv)
  (match exp
    [(? symbol?)
     ;=>
     (hash-ref aenv exp)]
    
    [(? integer?)
     ;=>
     (alpha exp)]
    
    [`(+ ,exp1 ,exp2)
     ;=>
     (+/abstract (exp-aeval exp1 aenv)
                 (exp-aeval exp2 aenv))]
    
    [`(* ,exp1 ,exp2)
     ;=>
     (*/abstract (exp-aeval exp1 aenv)
                 (exp-aeval exp2 aenv))]
        
    [`(= ,exp1 ,exp2)
     ;=>
     {set 0 '+}]))


(define (astate->string astate)
  
  (if (pair? (astate-stmts astate))
      (format "stmt: ~a~nenv:  ~a~n" 
              (car (astate-stmts astate))
              (astate-aenv astate))
      "end~n"))
              
  

; astep : astate -> astate*
(define (astep astate0)
  
  (define stmts (astate-stmts astate0))
  
  (define aenv (astate-aenv astate0))
  
  (match stmts
    ['()
     ;=>
     aenv]
    
    [(cons `(label ,l) rest)
     ;=>
     (list (astate rest aenv))]
    
    [(cons `(:= ,var ,exp) rest)
     ;=>
     (define aenv* (hash-set aenv var (exp-aeval exp aenv)))
     (list (astate rest aenv*))]
    
    [(cons `(if ,exp goto ,label) rest)
     ;=>
     (define condition (exp-aeval exp aenv))
     (list
      (astate (hash-ref stmt-map label) aenv)
      (astate rest aenv))]
    
    [(cons `(goto ,label) rest)
     ;=>
     (list 
      (astate (hash-ref stmt-map label) aenv))]
         
    [else
     ;=>
     (error (format "unknown instruction: ~a!" (car stmts)))]))
    


(define (analyze prog)
  
  (preprocess prog)
  
  ; the initial abstract state:
  (define astate0 (ainject prog))
  
  ; the set of all states ever seen:
  (define visited (set))
  
  ; the neighbor maps
  (define neighbors (make-hasheq))
  
  ; mark the neigbors of a state:
  (define (mark-neighbors! astate succs)
    (hash-set! neighbors astate succs))
  
  ; marks a state as seen:
  (define (mark-seen! astate)
    (set! visited (set-add visited astate)))
  
  ; checks if a state is seen:
  (define (seen? astate)
    (set-member? visited astate))
  
  ; states to explore next:
  (define todo (list astate0))
  
  ; adds states to be explored:
  (define (push-todos astates)
    (set! todo (append astates todo)))
  
  ; grabs the next state to be explored:
  (define (pop-todo)
    (define next (car todo))
    (set! todo (cdr todo))
    next)
  
  (while (not (null? todo))
    (define curr (pop-todo))
    (when (not (seen? curr))
      (mark-seen! curr)
      (define succs (astep curr))
      (when (list? succs)
        (mark-neighbors! curr succs)
        (push-todos succs))))
  
  ; return all visited states:
  neighbors)
        
        
(define beast-number 0)
(define beast-map (make-hash))

(define (mark-of-beast object)
  (hash-ref beast-map object 
            (lambda ()
              (set! beast-number (+ 1 beast-number))
              (hash-set! beast-map object beast-number)
              beast-number)))



;;;;;

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

(analyze prog)
