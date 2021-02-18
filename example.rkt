#lang macro-as-type

(define x : Number 1)
x
(define (id-Number [x : Number]) : Number
  x)
(id-Number x)
; error case: pass String to Number
; > (id-Number "foo")

(define {A} (id [x : A]) : A
  x)
(id x)
