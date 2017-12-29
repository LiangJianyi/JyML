#lang racket
(require "../jyml-tokenizer.rkt")
(require "../jyml-parser.rkt")
(require compatibility/mlist)

(define (println arg)
  (display arg)
  (newline))

(define path "/Users/liangjianyi/Desktop/parse-text.jyml")
(define path2 "/Users/liangjianyi/Desktop/parse-text-2.jyml")

;(define ast [make-ast '* null])
;(set! ast (append-ast ast [make-ast null 12]))
;(set! ast (append-ast ast [make-ast '+ null]))
;(set! ast (append-ast ast [make-ast null 10]))
;(set! ast (append-ast ast [make-ast '/ null]))
;(set! ast (append-ast ast [make-ast 4 null]))
;(set! ast (append-ast ast [make-ast '- null]))
;(set! ast (append-ast ast [make-ast 100 98]))



;(define code-text "(define a 123)  (let ((x 1) (y 2)) (+ x y))")
(define code-text (read-jyml-file path))
(define ast (parse (mlist->list (tokenizer code-text))))

ast

;(set! code-text (read-jyml-file path2))
;(set! ast (parse (mlist->list (tokenizer code-text))))

'----------------------统计文本中的换行符
(let ([tally 0])
  (for ([c code-text])
    (when [equal? c #\newline]
      (set! tally (+ tally 1))))
  tally)

'--------------------------------------------------------------------------
(define cond-branch-a
  (list (list "equal?" "left" (list "car" "tokens"))
        (list "set!" "tokens" (list "cdr" "tokens"))
        (list "cons" (list "f") (list "f"))))

(define cond-branch-b
  (list (list "equal?" "right" (list "car" "tokens"))
        (list "set!" "tokens" (list "cdr" "tokens"))
        "null"))

(define cond-branch-c
  (list "else"
        (list "cons"
              (list "car" "tokens")
              (list "begin"
                    (list "set!" "tokens" (list "cdr" "tokens"))
                    (list "f")))))

(define if-branch
  (list "if"
        (list "null?" "tokens")
        "null"
        (list "cond" cond-branch-a cond-branch-b cond-branch-c)))

(define root
  (list (list "define"
              (list "parse" "tokens")
              (list "define" (list "f") if-branch)
              (list "f"))
        (list "define"
              (list "parse" "tokens")
              (list "define" (list "f") if-branch)
              (list "f"))
        ))

(equal? root
        ast)

'--------------------------------------------------------------------------
(define mylambda
  (list (list "define" "x" "100")
        (list (list "lambda" (list "n") (list "*" "n" "n"))
              "x")))

(define fuck (parse (mlist->list (tokenizer "(define x 100)([lambda (n) (* n n)] x)"))))
(equal? fuck mylambda)