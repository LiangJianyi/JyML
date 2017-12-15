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

(define (parse tokens)
  (define (f)
    (if [null? tokens]
        null
        [cond [(equal? #\( [car tokens])
               (set! tokens [cdr tokens])
               (cons (f) (f))]
              [(equal? #\) [car tokens])
               (set! tokens [cdr tokens])
               null]
              [else
               (cons [car tokens]
                     (begin
                       (set! tokens [cdr tokens])
                       (f)))]]))
  (f))

;(list->mlist '(1 2 3))
(define code-text "(define a 123)  (let ((x 1) (y 2)) (+ x y))")
(define ast (parse (mlist->list (tokenizer code-text))))

(define ast-2
  (cons (list "define" "a" "123")
        (cons
         (list "let" (list (list "x" "1") (list "y" "2")) (list "+" "x" "y"))
         null)))

(define ast-3
  (list (list "define" "a" "123")
        (list "let" (list (list "x" "1") (list "y" "2")) (list "+" "x" "y"))))