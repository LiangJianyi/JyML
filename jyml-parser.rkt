#lang racket
(require "./jyml-tokenizer.rkt")
(require "./jyml-ast.rkt")
(require liangjianyi-racket/linkedlist)
(require liangjianyi-racket/binarytree)
(require compatibility/mlist)

(define token-sequence (tokenizer "(+ (/ 100 10) (* 2 5))"))

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

;(define code-text "(define a 123)  (let ((x 1) (y 2)) (+ x y))")
(define code-text (let ([path "/Users/liangjianyi/desktop/parse-text.jyml"]
                        [f (lambda (iport)
                             (letrec ([foo (lambda (c lik)
                                             (if [eof-object? c]
                                                 (begin
                                                   (close-input-port iport)
                                                   (list->string lik))
                                                 (foo (read-char iport) (append lik (list c)))))])
                               (foo (read-char iport) null)))])
                    (f (open-input-file path))))
(define ast (parse (mlist->list (tokenizer code-text))))

(define (read-jyml-file path)
  (letrec ([iport (open-input-file path)]
           [f (lambda (c lik)
                (if [eof-object? c]
                    (begin
                      (close-input-port iport)
                      lik)
                    (f (read-char iport) (string-append lik (string c)))))])
    (f (read-char iport) "")))

(define ast-2
  (cons (list "define" "a" "123")
        (cons
         (list "let" (list (list "x" "1") (list "y" "2")) (list "+" "x" "y"))
         null)))

(define ast-3
  (list (list "define" "a" "123")
        (list "let" (list (list "x" "1") (list "y" "2")) (list "+" "x" "y"))))