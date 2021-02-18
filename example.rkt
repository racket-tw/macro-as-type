#lang macro-as-type

(define x : Number 1)
(define (id-Number [x : Number]) : Number
  x)

x
(id-Number x)

(define s : String "foo")
(id-Number s)

#;(define {A} (id [x : A]) : A
  x)
