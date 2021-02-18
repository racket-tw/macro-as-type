#lang racket

(provide (except-out (all-from-out racket)
                     define
                     #%app)
         (rename-out [define- define]
                     [app #%app]))

(require syntax/parse/define
         (for-syntax racket/match))

(begin-for-syntax
  (struct FuncType (param-ty* ret-ty) #:transparent)

  (define (<-type stx)
    (syntax-parse stx
      [x:number 'Number]
      [x:string 'String]
      [x:boolean 'Boolean]
      [x:char 'Char]
      [_ (eval stx)]))

  (define (unify expect-ty actual-ty [expr #f] [sub-expr #f])
    (unless (equal? expect-ty actual-ty)
      (raise-syntax-error 'type-mismatched
                          (format "expected `~a`, but got `~a`" expect-ty actual-ty)
                          expr
                          sub-expr))))

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
       (define-for-syntax name 'ty)
       (define name exp))]
  [(_ {generic*:id ...} (name:id [p* : ty*] ...) : ty body)
   (unify (syntax->datum #'ty)
          (<-type #'(let ([generic* 'generic*] ...)
                      (let ([p* 'ty*] ...)
                        body)))
          this-syntax #'body)
   #'(begin
       (define-for-syntax name (FuncType '(ty* ...) 'ty))
       (define (name p* ...)
         body))]
  [(_ (name:id [p* : ty*] ...) : ty body)
   (unify (syntax->datum #'ty)
          (<-type #'(let ([p* 'ty*] ...)
                      body))
          this-syntax #'body)
   #'(begin
       (define-for-syntax name (FuncType '(ty* ...) 'ty))
       (define (name p* ...)
         body))])

(module reader syntax/module-reader
  macro-as-type)
