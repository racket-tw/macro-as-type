#lang racket

(module exp racket
  (provide (rename-out [define- define]
                       [app #%app]))
  (require syntax/parse/define
           (for-syntax racket/match))

  (begin-for-syntax
    (struct FuncType (param-ty* ret-ty) #:transparent)

    (define (<-type stx)
      (syntax-parse stx
        [x:id (eval #'x)])))

  (define-syntax-parser app
    [(_ f:expr arg*:expr ...)
     (define f-ty (<-type #'f))
     (cond
       [(FuncType? f-ty)
        (unless (equal? (FuncType-param-ty* f-ty) (map <-type (syntax->list #'(arg* ...))))
          (error 'type-mismatched))]
       [else (error 'apply-on-non-function)])
     #'(f arg* ...)])

  (define-syntax-parser define-
    #:datum-literals (:)
    [(_ name:id : ty exp)
     #'(begin
         (define-for-syntax name 'ty)
         (define name exp))]
    [(_ (name:id [p* : ty*] ...) : ty body)
     #'(begin
         (define-for-syntax name (FuncType '(ty* ...) 'ty))
         (define (name p* ...)
           body))]))

(require 'exp)

(define x : Number 1)
(define (foo [x : Number]) : Number
  x)

x
(foo x)
