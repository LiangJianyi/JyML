#lang racket
(require "../jyml-tokenizer.rkt")
(require "../jyml-parser.rkt")
(require "../jyml-interpreter.rkt")
(require "../exec-enviroment.rkt")
(require compatibility/mlist)

(define path "")
(define code-text (read-jyml-file path))
(define ast (parse (mlist->list (tokenizer code-text))))