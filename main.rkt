#lang racket

(provide (except-out (all-from-out racket)
                     define
                     #%app)
         (rename-out [define- define]
                     [app #%app])
         claim
         (for-syntax Number
                     String
                     Boolean
                     Char
                     ->
                     List
                     zero+))

(require syntax/parse/define
         (for-syntax racket/match
                     racket/list
                     (rename-in racket/base
                                [list racket-list])
                     "type.rkt"))

(begin-for-syntax
  (define Number 'Number)
  (define String 'String)
  (define Boolean 'Boolean)
  (define Char 'Char)
  (define (-> . arg*)
    (let-values ([(param* ret) (split-at-right arg* 1)])
      (for ([param-ty (drop-right param* 1)]
            [p param*])
        (when (*Type? param-ty)
          (error 'semantic "only last parameter can expect arbitrarily arguments")))
      (FuncType param* (first ret))))

  (define (List element-type)
    (HigherType 'List (list element-type)))
  (define (zero+ element-type)
    (*Type element-type))

  (define (check-app stx)
    (syntax-parse stx
      [(f:expr arg*:expr ...)
       (define f-ty (<-type #'f))
       (cond
         [(FuncType? f-ty)
          (define param-ty* (FuncType-param-ty* f-ty))
          (define argument* (syntax->list #'(arg* ...)))
          (define subst-map (make-hash))
          (unless (or (= (length param-ty*) (length argument*))
                      (*Type? (last param-ty*)))
            (raise-syntax-error 'arity
                                (format "need ~a but get ~a"
                                        (length param-ty*)
                                        (length argument*))
                                stx
                                #'(arg* ...)))
          ; FIXME: *Type cannot check arguments correctly since for-loop here
          (for ([param-ty param-ty*]
                [arg argument*])
            (define arg-ty (<-type arg))
            (unify param-ty arg-ty
                   stx arg
                   #:subst-map subst-map))
          (FuncType-ret-ty f-ty)]
         [else (error 'apply-on-non-function)])]))

  (define (<-type stx)
    (syntax-parse stx
      [x:number 'Number]
      [x:string 'String]
      [x:boolean 'Boolean]
      [x:char 'Char]
      [(λ (p*:id ...) body)
       (FuncType (map <-type (syntax->list #'(p* ...)))
                 (<-type #'(let ([p* (FreeVar (gensym 'λ))] ...)
                             body)))]
      [(let b* e)
       (eval stx)]
      [(f arg* ...)
       (check-app #'(f arg* ...))]
      [_ (eval stx)]))

  (define (unify expect-ty actual-ty [expr #f] [sub-expr #f]
                 #:subst-map [subst-map (make-hash)])
    (match* {expect-ty actual-ty}
      [{(? FreeVar?) t}
       (define existed? (hash-ref subst-map expect-ty #f))
       (cond
         [(not existed?)
          (hash-set! subst-map expect-ty actual-ty)]
         [(or (FreeVar? existed?)
              (FreeVar? actual-ty))
          (unify existed? actual-ty expr sub-expr
                 #:subst-map subst-map)]
         [else (raise-syntax-error
                'type-mismatched
                (format "expected ~a, but got ~a"
                        existed?
                        actual-ty)
                expr
                sub-expr)])]
      [{t (? FreeVar?)}
       (unify actual-ty expect-ty expr sub-expr
              #:subst-map subst-map)]
      [{(FuncType p1* ret1) (FuncType p2* ret2)}
       (map (λ (p1 p2)
              (unify p1 p2 expr sub-expr
                     #:subst-map subst-map))
            p1* p2*)
       (unify ret1 ret2 expr sub-expr
              #:subst-map subst-map)]
      [{(HigherType name1 ty1*) (HigherType name2 ty2*)}
       (unify name1 name2 expr sub-expr
              #:subst-map subst-map)
       (map (λ (t1 t2)
              (unify t1 t2 expr sub-expr
                     #:subst-map subst-map))
            ty1* ty2*)]
      [{(*Type ty) t2}
       (unify ty t2 expr sub-expr
              #:subst-map subst-map)]
      [{t1 (*Type ty)}
       (unify ty t1 expr sub-expr
              #:subst-map subst-map)]
      [{_ _}
       (unless (equal? expect-ty actual-ty)
         (raise-syntax-error 'type-mismatched
                             (format "expected `~a`, but got `~a`" expect-ty actual-ty)
                             expr
                             sub-expr))]))

  (define-syntax-class type
    (pattern x:id)
    (pattern (-> p*:type ... r:type))))

(define-syntax-parser app
  [(_ f:expr arg*:expr ...)
   (check-app #'(f arg* ...))
   #'(f arg* ...)])

(define-syntax-parser define-
  #:datum-literals (:)
  [(_ name:id : ty:type exp)
   #'(define- {} name : ty exp)]
  [(_ {generic*:id ...} name:id : ty:type exp)
   (unify (eval #'(let ([generic* (FreeVar 'generic*)] ...)
                    ty))
          (<-type #'exp)
          this-syntax #'exp)
   #'(begin
       (define-for-syntax name
         (let ([generic* (FreeVar 'generic*)] ...)
           ty))
       (define name exp))]
  [(_ (name:id [p*:id : ty*:type] ...) : ty:type body)
   (unify (syntax->datum #'ty)
          (<-type #'(let ([p* ty*] ...)
                      body))
          this-syntax #'body)
   #'(begin
       (define-for-syntax name (ty* ... . -> . ty))
       (define (name p* ...)
         body))]
  [(_ {generic*:id ...} (name:id [p*:id : ty*:type] ...) : ty:type body)
   (unify (eval #'(let ([generic* (FreeVar 'generic*)] ...)
                    ty))
          (<-type #'(let ([generic* (FreeVar 'generic*)] ...)
                      (let ([p* ty*] ...)
                        body)))
          this-syntax #'body)
   #'(begin
       (define-for-syntax name
         (let ([generic* (FreeVar 'generic*)] ...)
           (ty* ... . -> . ty)))
       (define (name p* ...)
         body))])

(define-syntax-parser claim
  #:datum-literals (:)
  [(_ name:id : ty:type)
   #'(claim {} name : ty)]
  [(_ {generic*:id ...} name:id : ty:type)
   #'(define-for-syntax name
       (let ([generic* (FreeVar 'generic*)] ...)
         ty))])

(module reader syntax/module-reader
  macro-as-type)
