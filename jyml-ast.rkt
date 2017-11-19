#lang racket

(provide make-ast)
(provide iterator-ast)
(provide append-ast)
(provide append-astnode)

(define (make-ast left right)
  (mcons left right))

(define (iterator-ast ast proc)
  (proc ast)
  (unless [mpair? ast]
    (iterator-ast [mcar ast] proc)
    (iterator-ast [mcdr ast] proc)))

(define (append-ast ast obj)
  (if [mpair? ast]
      (make-ast [append-ast (mcar ast) obj] [append-ast (mcdr ast) obj])
      (if [null? ast]
          obj
          ast)))

(define (append-astnode ast obj)
  (if [mpair? ast]
      (cond [(null? [mcar ast]) (make-ast (append-astnode [mcar ast] obj) [mcdr ast])]
            [(null? [mcdr ast]) (make-ast [mcar ast] (append-astnode [mcdr ast] obj))]
            [else ast])
      (if [null? ast]
          obj
          ast)))

;(define (copy-ast ast)
;  ())

