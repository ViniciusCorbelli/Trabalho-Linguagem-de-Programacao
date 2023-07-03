#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/classes/ast)

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
    [(ast:int n) n]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:proc (ast:var v) e) (proc-val v e Δ)]
    [(ast:call e1 e2) (apply-proc (value-of e1 Δ) (value-of e2 Δ))] ; call by value
    [(ast:letrec (ast:var f) (ast:var v) e1 e2) (value-of e2 (extend-env-rec f v e1 Δ))]
    [(ast:begin es) (foldl (lambda (e v) (value-of e Δ)) (value-of (first es) Δ) (rest es))]
    [(ast:assign (ast:var x) e) (begin
                                  (setref! (apply-env Δ x) (value-of e Δ))  ;set the value in the store
                                  42)]   ; return the 42 value
    [(ast:new class args) (let ([args (values-of-exps args Δ)]   
                              [obj (new-object (ast:var-name class))])         
                                  (let ([this-meth (find-method (object-class-name obj) "initialize")])  
                                    (apply-method 
                                      this-meth
                                      obj
                                      args))
                                  obj)]
    [(ast:self) (apply-env Δ '%self)]
    [(ast:send object method args) (let ([args (values-of-exps args Δ)] 
                                         [obj (value-of object Δ)])
                                     (apply-method 
                                      (find-method (object-class-name obj) (ast:var-name method))
                                      obj
                                      args))]
    [(ast:super name args) (let ([args (values-of-exps args Δ)] 
                                          [obj (apply-env Δ '%self)])
                                      (apply-method
                                       (find-method (apply-env Δ '%super) (ast:var-name name))
                                       obj
                                       args))]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))


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
      (if maybe-pair (cadr maybe-pair)
          (display "Classe desconhecida\n")))))

;Inicializar uma lista de pares que representa o nome da classe e descricao da classe
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list "object" (class #f '() '())))) 
    (for-each initialize-class-decl! c-decls)))


;Inicializa as declaracoes da classe (nome,metodo,nome da classe que estende e campos)
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
             c-methods c-super c-fields)))))
    )
)

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
(define values-of-exps
  (lambda (exps env)
    (map
     (lambda (exp) (value-of exp env))
     exps)))

;Concatena as listas em uma lista só
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (zip  (cdr l1) (cdr l2))))
)

; Boot da aplicação onde vai comecar a execução do programa
; Começando o ambiente de classes passando as declarações
; Avalia a expressão em um ambiente vazio
(define (value-of-program prog) 
   (match-define (ast:prog decl exp) prog)
   (initialize-class-env! decl)
    ; you must collect all the classes declared and building its respectively environment
    ; execute the prog expression in the correct environment
  (value-of exp init-env))