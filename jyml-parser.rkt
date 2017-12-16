#lang racket
(require "./jyml-tokenizer.rkt")
;(require "./jyml-ast.rkt")
;(require liangjianyi-racket/linkedlist)
;(require liangjianyi-racket/binarytree)
(require compatibility/mlist)

(provide parse)

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

(define (read-jyml-file path)
  (letrec ([iport (open-input-file path)]
           [f (lambda (c lik)
                (if [eof-object? c]
                    (begin
                      (close-input-port iport)
                      lik)
                    (f (read-char iport) (string-append lik (string c)))))])
    (f (read-char iport) "")))

