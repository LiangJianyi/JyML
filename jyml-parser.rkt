#lang racket
(require "./jyml-tokenizer.rkt")
(require "./jyml-ast.rkt")
(require liangjianyi-racket/linkedlist)
(require liangjianyi-racket/binarytree)
(require compatibility/mlist)

(define token-sequence (tokenizer "(+ (/ 100 10) (* 2 5))"))

;(define (parse tokens)
;  ())

(define (println arg)
  (display arg)
  (newline))

;(define ast [make-ast '* null])
;(set! ast (append-ast ast [make-ast null 12]))
;(set! ast (append-ast ast [make-ast '+ null]))
;(set! ast (append-ast ast [make-ast null 10]))
;(set! ast (append-ast ast [make-ast '/ null]))
;(set! ast (append-ast ast [make-ast 4 null]))
;(set! ast (append-ast ast [make-ast '- null]))
;(set! ast (append-ast ast [make-ast 100 98]))

(define (parse tokens [get-tokens false])
  (unless [null? tokens]
    [cond [(equal? #\( [car tokens])
           (cons
            (let ([result (parse [cdr tokens] true)])
              (set! tokens (result 'token))
              (result 'content))
            (parse [cdr tokens]))]
          [(equal? #\) [car tokens])
           (parse [cdr tokens])]
          [else (if [equal? get-tokens true]
                    (lambda (dispatch)
                      (cond [(eq? 'content dispatch) [car tokens]]
                            [(eq? 'token dispatch) tokens]))
                    [car tokens])]]))

;(list->mlist '(1 2 3))
(define code-text "(define a 123)  (let ([x 1] [y 2]) (+ x y))")
(define ast (parse (mlist->list (tokenizer code-text))))
