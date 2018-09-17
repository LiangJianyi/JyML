#lang racket
(require "../jyml-tokenizer.rkt")
(require "../jyml-parser.rkt")
(require "../jyml-interpreter.rkt")
(require "../exec-enviroment.rkt")
(require compatibility/mlist)
(require liangjianyi-racket/BinaryTree)

(define path "jyml-text.jyml")
(define code-text (read-jyml-file path))
(define ast (parse (mlist->list (tokenizer code-text))))
(define type-ast (binarytree-map ast inferred-type))
(eval type-ast)

(+ 1 2)
(+ 100 100)
-1
1.111