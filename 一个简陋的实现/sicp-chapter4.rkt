#lang racket
(require compatibility/mlist)

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (map proc items)
    (if [null? items]
        null
        (mcons (proc [mcar items])
                (map proc [mcdr items]))))

(define (tagged-list? exp tag)
    (if (mpair? exp)
        (eq? (mcar exp) tag)
        false))

(define (eval exp env)
    (cond   [[self-evaluating? exp] exp]
            [[variable? exp] (lookup-variable-value exp env)]
            [[quoted? exp] (text-of-quotation exp)]
            [[assignment? exp] (eval-assignment exp env)]
            [[definition? exp] (eval-definition exp env)]
            [[if? exp] (eval-if exp env)]
            [[lambda? exp]
            (make-lambda    (lambda-parameters exp)
                            (lambda-body exp)
                            env)]
            [[begin? exp] (eval-sequence (begin-actions exp) env)]
            [[cond? exp] (eval (cond->if exp) env)]
            [[application? exp] 
            (apply  (eval (operator exp) env) 
                    (list-of-values (operands exp) env))]
            [else (error "Unknow expression type -- eval" exp)]))

(define (apply procedure arguments)
    (cond   [(primitive-procedure? procedure)
            (apply-primitive-procedure procedure arguments)]
            [(compound-procedure? procedure)
            (eval-sequence (procedure-body procedure) (extend-enviroment
                                                        (procedure-parameters procedure)
                                                        arguments
                                                        (procedure-enviroment procedure)))]
            [else (error "Unknown procedure type -- Apply" procedure)]))

;; eval 在处理过程应用时用 list-of-values 去生成实际参数表，以便完成这一过程应用。
;; list-of-values 以组合式的运算对象作为参数，求值各个运算对象，返回这些值的表
(define (list-of-values exps env)
    (if [no-operands? exps]
        null
        (mcons  (eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))

;; eval-if 在给定环境中求值 if 表达式的谓词部分，如果得到的结果为真，eval-if 就去求值这个 if 的
;; consequent 部分，否则求值其 alternative 部分
(define (eval-if exp env)
    (if [true? (eval (if-predicate exp) env)]
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

;; eval-sequence 用在 apply 里，用于求值过程体里的表达式序列。它也用在 eval 里，用于
;; 求值 begin 表达式里的表达式序列。这个过程以一个表达式序列和一个环境为参数，按照序列里
;; 的表达式出现的顺序对它们求值。它返回最后一个表达式的值。
(define (eval-sequence exps env)
    (cond   [(last-exp? exps) (eval (first-exp exps) env)]
            [else   (eval (first-exp exps) env)
                    (eval-sequence (rest-exps exps) env)]))

;; eval-assignment 调用 eval 找出需要赋的值，将变量和得到的值传给过程 set-variable-value!，
;; 将有关的值安置到指定环境里
(define (eval-assignment exp env)
    (set-variable-value!    (assignment-variable exp)
                            (eval (assignment-value exp) env)
                            env)
    'ok)

;; 变量定义也用类似的方法处理
(define (eval-definition exp env)
    (define-variable!   (definition-variable exp)
                        (eval (definition-value exp) env)
                        env))

(define (self-evaluating? exp)
    (cond   [(number? exp) true]
            [(string? exp) true]
            [else false]))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) [mcar [mcdr exp]])

;;;;;;;;;;;;;;;;;;;赋值的形式：(set! <var> <value>)

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) [mcar [mcdr exp]])

(define (assignment-value exp) [mcar [mcdr [mcdr exp]]])

;;定义的形式：(define <var> <value>)
;;或者：(define (<var> <parameter 1>...<parameters n>) <body>) 此形式是下面形式的语法糖
;; (define <var>
;;      (lambda (<parameter 1>...<parameters n>)
;;          <body>))

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp) 
    (if [symbol? [mcar [mcdr exp]]]
        [mcar [mcdr exp]]
        [mcar [mcar [mcdr exp]]]))

(define (definition-value exp) 
    (if [symbol? [mcar [mcdr exp]]]
        [mcar [mcdr [mcdr exp]]]
        (make-lambda    [mcdr [mcar [mcdr exp]]]    ; formal parameters
                        [mcdr [mcdr exp]])))        ; body

;;;;;;;;;;;;;;;;;;;lambda表达式是由符号lambda开始的表：

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) [mcar [mcdr exp]])

(define (lambda-body exp) [mcdr [mcdr exp]])

(define (make-lambda parameters body)
    (mcons 'lambda (mcons parameters body)))

;;;;;;;;;;;;;;;;;;;条件式由if开始，有一个谓词部分、一个推论部分和一个替代部分（可省略）。
;;;;;;;;;;;;;;;;;;;如果这一表达式没有替代部分，就以 false 作为其返回值

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) [mcar [mcdr exp]])

(define (if-consequent exp) [mcar [mcdr [mcdr exp]]])

(define (if-alternative exp)
    (if [not (null? [mcdr [mcdr [mcdr exp]]])]
        [mcar [mcdr [mcdr [mcdr exp]]]]
        'false))

(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

;;;;;;;;;;;;;;;;;;;begin 包装起一个表达式序列，在这里提供了对begin表达式的一组语法操作，以便从
;;;;;;;;;;;;;;;;;;;begin 表达式中提取出实际表达式序列，还有选择器返回的序列中的第一个表达式和其余表达式

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) [mcdr exp])

(define (last-exp? seq) (null? [mcdr seq]))

(define (first-exp seq) [mcar seq])

(define (rest-exps seq) [mcdr seq])

(define (make-begin seq) [mcons 'begin seq])

(define (sequence->exp seq)
    (cond   [(null? seq) seq]
            [(last-exp? seq) (first-exp seq)]
            [else (make-begin seq)]))

;; 过程 application 就是不属于上述各种表达式类型的任意复合表达式，
;; 这种表达式的 mcar 是运算符，其 mcdr 是运算对象的表

(define (application? exp) (mpair? exp))

(define (operator exp) (mcar exp))

(define (operands exp) (mcdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) [mcar ops])

(define (rest-operands ops) [mcdr ops])

;; 一些特殊的语法形式可以基于其它的语法形式的表达式定义出来，而不必直接去实现 cond 便可以由一些嵌套的 if 表达式配合 begin 实现出来，
;; cond 包含若干个谓词，每个谓词对应若干个动作子句组成的表；如果一个子句的符号是 else，那么就是一个 else 子句

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) [mcdr exp])

(define (cond-else-clauses? clause) (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) [mcar clause])

(define (cond-actions clause) [mcdr clause])

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
    (if [null? clauses]
        'false
        (let (  [first [mcar clauses]]
                [rest [mcdr clauses]])
            (if (cond-else-clauses? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "else clause isn't last -- cond->if" clauses))
                (make-if    (cond-predicate first)
                            (sequence->exp (cond-actions first))
                            (expand-clauses rest))))))

;;;;;;;;;;;;;;;;;;;过程的表示
(define (make-procedure parameters body env)
    (mlist 'procedure parameters body env))

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (procedure-parameters p) [mcar [mcdr p]])

(define (procedure-body p) [mcar [mcdr [mcdr p]]])

(define (procedure-enviroment p) [mcar [mcdr [mcdr [mcdr p]]]])

(define (enclosing-enviroment env) [mcdr env])

(define (first-frame env) [mcar env])

(define the-empty-enviroment null)

(define (make-frame variables values)
    (mcons variables values))

(define (frame-variables frame) [mcar frame])

(define (frame-values frame) [mcdr frame])

(define (add-binding-to-frame! var val frame)
    (set-mcar! frame (mcons var [mcar frame]))
    (set-mcdr! frame (mcons val [mcdr frame])))

(define (extend-enviroment variables values base-env)
    (if [= (length variables) (length values)]
        (mcons (make-frame variables values) base-env)
        (if [< (length variables) (length values)]
            (error "Too many arguments supplied" variables values)
            (error "Too few arguments supplied" variables values))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan variables values)
            (cond   ([null? variables]
                    (env-loop [enclosing-enviroment env]))
                    ([eq? var [mcar variables]]
                    [mcar variables])
                    (else (scan [mcdr variables] [mcdr values]))))
        (if [eq? env the-empty-enviroment]
            (error "Unbound variable" var)
            (let ([frame (first-frame env)])
                (scan   (frame-variables frame)
                        (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan variables values)
            (cond   ([null? variables]
                    (env-loop [enclosing-enviroment env]))
                    ([eq? var [mcar variables]]
                    [set-mcar! values val])
                    (else (scan [mcdr variables] [mcdr values]))))
        (if [eq? env the-empty-enviroment]
            (error "Unbound variable -- set!" var)
            (let ([frame (first-frame env)])
                (scan   (frame-variables frame)
                        (frame-values frame)))))
    (env-loop env))

(define (define-variable! var val env)
    (let ([frame (first-frame env)])
        (define (scan variables values)
            (cond   [(null? variables)
                    (add-binding-to-frame! var val frame)]
                    [(eq? var [mcar variables])
                    (set-mcar! values val)]
                    [else (scan [mcdr variables] [mcdr values])]))
        (scan   (frame-variables frame)
                (frame-values frame))))

(define (setup-enviroment)
    (let ((initial-env (extend-enviroment (primitive-procedure-names) 
                                        (primitive-procedure-objects) 
                                        the-empty-enviroment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (mcar (mcdr proc)))

(define primitive-procedures (mlist (mlist 'car car)
                                    (mlist 'cdr cdr)
                                    (mlist 'cons cons)
                                    (mlist 'null? null?)
                                    ;;
                                    ))

(define (primitive-procedure-names)
    (map mcar primitive-procedures))

(define (primitive-procedure-objects)
    (map (lambda (proc) (mlist 'primitive (mcar (mcdr proc))))
        primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args))

