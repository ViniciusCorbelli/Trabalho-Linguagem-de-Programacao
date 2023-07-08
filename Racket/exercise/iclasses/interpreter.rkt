#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; Representação de procedimentos para escopo estático
; ----------------- STRUCTS ------------------------------
(struct object (class-name fields))
(struct class (super-name fields-names methods))
(struct method (super-name params body fields))

;Define o ambiente de classes como um ambiente inicialmente vazio
(define the-class-env '())

; proc-val :: Var x Expr x Env -> Proc
(define (proc-val var exp Δ) ; call by value
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
(define (apply-proc proc val)
  (proc val))

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var (ast:var x))
      (apply-env Δ (ast:var-name x))]
    [(ast:send object (ast:var mth) args)
      (let* ([method-name (ast:var-name mth)]
            [args (values-of-exps args Δ)]
            [obj (value-of object Δ)])
        (apply-method (find-method (object-class-name obj) method-name) obj args))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send object method args)
      (let ([method-name (ast:var-name method)]
            [args (values-of-exps args Δ)]
            [obj (value-of object Δ)])
        (apply-method (find-method (object-class-name obj) method-name) obj args))]
    [(ast:super name args) (let ([args (values-of-exps args Δ)] 
                                          [obj (apply-env Δ '%self)])
                                      (apply-method
                                       (find-method (apply-env Δ '%super) (ast:var-name name))
                                       obj
                                       args))]
    [(ast:self) (apply-env Δ '%self)]
    [(ast:new class args)
     (let ([class-name (ast:var-name class)])
       (let* ([class (lookup-class class-name)])
         (if class
             (let ([args (values-of-exps args Δ)]
                   [obj (new-object class-name)])
               (let ([initialize-method (find-method class-name "initialize")])
                 (if initialize-method
                     (apply-method initialize-method obj args)
                     obj)))
             (raise-user-error "Unknown class: " class-name))))]
    [e (raise-user-error "unimplemented-construction 1: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e)
        (let ([value (value-of e Δ)])
          (extend-env x value Δ))]
    [(ast:print e) (display (value-of e Δ))]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts)
        (let ([block-env Δ]) ; Cria um ambiente local para o bloco
          (for-each (lambda (s) (set! block-env (result-of s block-env))) stmts) ; Executa cada stmt no ambiente local
          block-env)] ; Retorna o ambiente resultante
    [(ast:if-stmt e s1 s2)
        (if (value-of e Δ)
            (result-of s1 Δ)
            (result-of s2 Δ))]
    [(ast:while e s)
        (let loop ()
          (if (value-of e Δ)
              (begin
                (result-of s Δ)
                (loop))
              'done))]
     [(ast:local-decl (ast:var x) s)
      (let ([value (value-of-new s Δ)])
        (let ([new-env (extend-env x value Δ)])
          (result-of s new-env)))]
    [(ast:send object (ast:var mth) args)
      (let* ([method-name (ast:var-name mth)]
            [args (values-of-exps args Δ)]
            [obj (value-of object Δ)])
        (apply-method (find-method (object-class-name obj) method-name) obj args))]
    [(ast:super (ast:var c) args)
      (let* ([class-name (ast:var-name c)]
            [args (values-of-exps args Δ)]
            [obj (apply-env Δ '%self)])
        (apply-method (find-method (apply-env Δ '%super) class-name) obj args))]
    [e (raise-user-error "unimplemented-construction 2: " e)]
    ))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

; ------------------------------- NEW -------------------------------------
; class-fields :: Class -> [Var]
(define (class-fields class)
  (class-fields-names class))

; value-of-new :: Exp -> Env -> ExpVal
(define (value-of-new exp Δ)
  (match exp
    [(ast:new class args)
     (let* ([class-name (ast:var-name class)]
            [class (lookup-class class-name)])
       (if class
           (let ([args (values-of-exps args Δ)])
             (create-object class-name args))
           (raise-user-error "Unknown class: " class-name)))]
    [e (raise-user-error "Invalid new expression: " e)]))

; create-object :: ClassName x [ExpVal] -> ExpVal
(define (create-object class-name args)
  (let ([fields (class-fields (lookup-class class-name))])
    (object class-name (map newref fields))))


; ----------------------------- OBJETO ------------------------------------
;Construção de um novo objeto pegando o nome de classe e os campos
(define new-object      
  (lambda (class-name)
    (object             
      class-name        
      (map
       (lambda (field-name)
         (newref field-name))
       (class-fields-names (lookup-class class-name))))))


; ----------------------------- METODO ------------------------------------
; Executa um método de um objeto dentro de um ambiente composto pelas propriedades
; da classe e da superclasse
(define (apply-method m self args) 
  (if (method? m) 
        (let ([vars (method-params m)]
              [body (method-body m)]
              [super-name (method-super-name m)]
              [fields (method-fields m)])
          (value-of body (multiple-extend-env vars (map newref args) 
                                        (extend-env-with-self-and-super 
                                         self super-name
                                         (multiple-extend-env fields (object-fields self) 
                                                     empty-env)))))
                                                     (display "Método pedido não é método\n")))


; ------------------------------- AMBIENTE ----------------------------------
;Adiciona classe no ambiente de classes
(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
          (cons
           (list class-name class)
           the-class-env))))

; Busca uma classe pelo nome dentro do ambiente de classes
(define lookup-class    
  (lambda (name)
    (let ((maybe-pair (assoc name the-class-env)))
      (if maybe-pair
          (cadr maybe-pair)
          (display "Classe desconhecida\n")))))

;Inicializar uma lista de pares que representa o nome da classe e descrição da classe
(define (initialize-class-env! c-decls)
  (set! the-class-env '()) ; Limpa o ambiente de classes antes de adicionar as declarações

  ; Adiciona a classe "object" inicialmente
  (add-to-class-env! "object" (class #f '() '()))

  ; Inicializa as declarações das classes
  (for-each initialize-class-decl! c-decls))

;Inicializa as declarações da classe (nome, método, nome da classe que estende e campos)
(define (initialize-class-decl! c-decl)
  (let ([c-name (ast:var-name (ast:decl-name c-decl))]
        [c-methods (ast:decl-methods c-decl)]
        [c-super (ast:var-name (ast:decl-super c-decl))]
        [c-fields (map ast:var-name (ast:decl-fields c-decl))])
    (let ([c-fields
           (append-field-names
            (class-fields-names (lookup-class c-super))
            c-fields)])
      (add-to-class-env! c-name
                         (class c-super c-fields
                                (merge-method-envs
                                 (class-methods (lookup-class c-super))
                                 (method-decls-method-env
                                  c-methods c-super c-fields)))))))

; Adiciona as propriedades da superclasse junto das propriedades da classe
(define append-field-names 
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields) 
      (else                             
       (cons
        (if (memq (car super-fields) new-fields) 
            (fresh-identifier (car super-fields)) 
            (car super-fields))
        (append-field-names   
         (cdr super-fields) new-fields))))))


; ------------------------- AMBIENTE METODOS ---------------------------

; Vai procurar os metodos pelo nome do metodo e da classe
(define find-method
  (lambda (c-name name)
    (let ([this-class (lookup-class c-name)])   
      (if (void? this-class) (display "Classe não encontrada\n")
          (let ([m-env (class-methods this-class)])    
             (let ([maybe-pair (assoc name m-env)])      
               (if (pair? maybe-pair) (cadr maybe-pair)  
                   (display "Método não encontrado\n"))))))))


;Efetua merge no ambiente da classe e da super
(define merge-method-envs 
  (lambda (super-m-env new-m-env1) 
    (append new-m-env1 super-m-env)))


;Pega as declarações de método de uma classe e cria um ambiente de metodo, gravando para cada metodo suas variáveis vinculadas, corpo, nome da super classe e campos.
(define method-decls-method-env 
  (lambda (m-decls super-name field-names)  
    (map
     (lambda (m-decl)  
       (let ([method-name (ast:var-name(ast:method-name m-decl))] 
             [vars (map ast:var-name (ast:method-params  m-decl))]
             [body (ast:method-body m-decl)])
         (list method-name (method super-name vars body field-names)))) 
     m-decls)))


; Criação de ambiente estendido com procedimento recursivo
(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))


;Estende env da classe com a que ela herda metodos
(define (extend-env-with-self-and-super self super-name saved-env) ; função faz a ligação de %self e %super com um objeto e um nome de classe, respectivamente
  (lambda (svar)
    (case svar
            ((%self) self)
            ((%super) super-name)
            (else (apply-env saved-env svar)))))


;Implementa o extend-env em uma lista
(define (multiple-extend-env vars values env) 
 (foldl (lambda (var-value env) (extend-env (first var-value) (second var-value) env)) env (zip vars values))
)


;Qualquer campo da superclasse que é sombreado por um novo campo é substituído por um novo identificador
(define fresh-identifier 
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"
        (number->string sn))))))

;Avalia o valor das expressoes numa lista
(define (values-of-exps exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))

;Concatena as listas em uma lista só
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (zip  (cdr l1) (cdr l2))))
)

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       (initialize-class-env! decls) ; Inicializa o ambiente de classes
       (result-of stmt init-env))]))

