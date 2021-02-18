#lang racket

(provide (except-out (all-from-out racket)
                     define
                     #%app)
         (rename-out [define- define]
                     [app #%app])
         (for-syntax Number String Boolean Char))

(require syntax/parse/define
         (for-syntax racket/match))

(begin-for-syntax
  (struct FuncType (param-ty* ret-ty)
    #:methods gen:custom-write
    [(define (write-proc f port mode)
       (fprintf port "~a -> ~a"
                (FuncType-param-ty* f)
                (FuncType-ret-ty f)))]
    #:transparent)
  (struct FreeVar (val)
    #:methods gen:custom-write
    [(define (write-proc f port mode)
       (fprintf port "?~a" (FreeVar-val f)))]
    #:transparent)

  (define Number 'Number)
  (define String 'String)
  (define Boolean 'Boolean)
  (define Char 'Char)

  (define (<-type stx)
    (syntax-parse stx
      [x:number 'Number]
      [x:string 'String]
      [x:boolean 'Boolean]
      [x:char 'Char]
      [_ (eval stx)]))

  (define (unify expect-ty actual-ty [expr #f] [sub-expr #f]
                 #:subst-map [subst-map (make-hash)])
    (match* {expect-ty actual-ty}
      [{(? FreeVar?) t}
       (hash-set! subst-map expect-ty actual-ty)]
      [{t (? FreeVar?)}
       (unify actual-ty expect-ty expr sub-expr)]
      [{_ _}
       (unless (equal? expect-ty actual-ty)
         (raise-syntax-error 'type-mismatched
                             (format "expected `~a`, but got `~a`" expect-ty actual-ty)
                             expr
                             sub-expr))])))

(define-syntax-parser app
  [(_ f:expr arg*:expr ...)
   (define f-ty (<-type #'f))
   (cond
     [(FuncType? f-ty)
      (for ([param-ty (FuncType-param-ty* f-ty)]
            [arg (syntax->list #'(arg* ...))])
        (define arg-ty (<-type arg))
        (unify param-ty arg-ty
               this-syntax arg))]
     [else (error 'apply-on-non-function)])
   #'(f arg* ...)])

(define-syntax-parser define-
  #:datum-literals (:)
  [(_ name:id : ty exp)
   (unify (syntax->datum #'ty) (<-type #'exp)
          this-syntax #'exp)
   #'(begin
       (define-for-syntax name ty)
       (define name exp))]
  [(_ {generic*:id ...} (name:id [p* : ty*] ...) : ty body)
   (unify (eval #'(let ([generic* (FreeVar 'generic*)] ...)
                    ty))
          (<-type #'(let ([generic* (FreeVar 'generic*)] ...)
                      (let ([p* ty*] ...)
                        body)))
          this-syntax #'body)
   #'(begin
       (define-for-syntax name
         (let ([generic* (FreeVar 'generic*)] ...)
           (FuncType (list ty* ...) ty)))
       (define (name p* ...)
         body))]
  [(_ (name:id [p* : ty*] ...) : ty body)
   (unify (syntax->datum #'ty)
          (<-type #'(let ([p* ty*] ...)
                      body))
          this-syntax #'body)
   #'(begin
       (define-for-syntax name (FuncType (list ty* ...) ty))
       (define (name p* ...)
         body))])

(module reader syntax/module-reader
  macro-as-type)
