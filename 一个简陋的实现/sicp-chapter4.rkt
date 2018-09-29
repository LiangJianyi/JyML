#lang racket
(require compatibility/mlist)

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
        [mcar [mcar [mcdr exp]]]]))

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

(define (map proc items)
    (if [null? items]
        null
        (mcons (proc [mcar items])
                (map proc [mcdr items]))))

(define (tagged-list? exp tag)
    (if (mpair? exp)
        (eq? (mcar exp) tag)
        false))

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

