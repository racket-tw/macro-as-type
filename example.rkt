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
(id "S")

(define id-Number2 : (Number . -> . Number)
  (λ (x) x))

(id-Number 1)

(define {A} id2 : (A . -> . A)
  (λ (x) x))

(id2 x)

(claim add1 : (Number . -> . Number))
(define y : Number
  (add1 2))
y

; error case: semantic: only last parameter can expect arbitrarily arguments
; > (claim asdklasfnasl : ((@ Number) Number . -> . (List Number)))
(claim {A} list : ((@ A) . -> . (List A)))
(define l0 : (List Number)
  (list 1 2 3))
l0
(list 1 2 3)
; error case: type-mismatched: expected `Number`, but got `String` in: "s"
; > (list 1 "s" 3)

(claim + : ((@ Number) . -> . Number))
(+ 1 2 3 4)
