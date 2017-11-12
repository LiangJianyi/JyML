#lang racket

(define tokenregular-whitelist
  (vector #\( #\)))

(define tokenregular-blacklist
  (vector #\space))

(define (tokenizer exp)
  (exp))

