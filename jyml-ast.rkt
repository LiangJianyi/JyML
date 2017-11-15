#lang racket

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

;(define (copy-ast ast)
;  ())