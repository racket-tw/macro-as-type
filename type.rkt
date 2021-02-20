#lang racket/base

(provide (struct-out FuncType)
         (struct-out HigherType)
         (struct-out FreeVar))

(struct FuncType (param-ty* ret-ty)
  #:methods gen:custom-write
  [(define (write-proc f port mode)
     (fprintf port "~a -> ~a"
              (FuncType-param-ty* f)
              (FuncType-ret-ty f)))]
  #:transparent)
(struct HigherType (name ty*)
  #:methods gen:custom-write
  [(define (write-proc h port mode)
     (fprintf port "(~a " (HigherType-name h))
     (for-each (λ (ty) (fprintf port "~a" ty))
               (HigherType-ty* h))
     (fprintf port ")"))]
  #:transparent)
(struct FreeVar (val)
  #:methods gen:custom-write
  [(define (write-proc f port mode)
     (fprintf port "?~a" (FreeVar-val f)))]
  #:transparent)
