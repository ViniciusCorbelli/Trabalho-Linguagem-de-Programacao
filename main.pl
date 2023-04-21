:- use_module(library(clpfd)).

% Predicado principal do jogo
jogar :-
    write('Escolha a versão do jogo: '),nl,
    write(1 - normal),nl,
    write(2 - implificada),nl,
    read(Versao),

    write('Digite a quantidade de linhas: '),nl,
    read(Largura),

    write('Digite a quantidade de colunas: '),nl,
    read(Altura),

    write('Digite a quantidade de peças repetidas para ganhar: '),nl,
    read(Consecutivas),

    criar_tabuleiro(Largura, Altura, Tabuleiro),
    imprimir_tabuleiro(Tabuleiro),
    jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao).

% Cria um tabuleiro vazio com o tamanho especificado
criar_tabuleiro(Largura, Altura, Tabuleiro) :-
    length(Tabuleiro, Altura),
    criar_linhas(Largura, Altura, Tabuleiro).

criar_linhas(_, 0, []).
criar_linhas(Largura, Altura, [Linha|Tabuleiro]) :-
    Altura > 0,
    length(Linha, Largura),
    Altura1 is Altura - 1,
    criar_linhas(Largura, Altura1, Tabuleiro).

% Imprime o tabuleiro
imprimir_tabuleiro(Tabuleiro) :-
    reverse(Tabuleiro, TabuleiroInvertido),
    imprimir_linhas(TabuleiroInvertido),
    nl.

imprimir_linhas([]).
imprimir_linhas([Linha|Tabuleiro]) :-
    write(' |'),
    imprimir_linha(Linha),
    write('|'),
    nl,
    imprimir_linhas(Tabuleiro).

imprimir_linha([]).
imprimir_linha([Celula|Linha]) :-
    (var(Celula) -> write('   |'); write(' '), write(Celula), write(' |')),
    imprimir_linha(Linha).

% Jogador 1 joga
jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao) :-
    write('Jogador 1 (X), escolha uma coluna: '),nl,
    read(Coluna),

    % Verifica se a coluna é válida
    (Coluna < 1 ; Coluna > Largura) ->
    write('Coluna inválida. Tente novamente.'), nl,
    jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao)
    ;

    criar_tabuleiro(Largura, Altura, NovoTabuleiro),
    jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, 'X', NovoTabuleiro),
    imprimir_tabuleiro(NovoTabuleiro),
    (
        vencedor(NovoTabuleiro, Consecutivas, 'X') ->
            write('Jogador 1 (X) venceu!'), nl
        ;
            empate(NovoTabuleiro) ->
                write('Empate!'), nl
            ;
                jogador2_joga(Largura, Altura, Consecutivas, NovoTabuleiro, Versao)
    ).

% Jogador 2 joga
jogador2_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao) :-
    write('Jogador 2 (O), escolha uma coluna: '),nl,
    read(Coluna),

    % Verifica se a coluna é válida
    (Coluna < 1 ; Coluna > Largura) ->
    write('Coluna inválida. Tente novamente.'), nl,
    jogador2_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao)
    ;

    jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, 'O', Tabuleiro),
    imprimir_tabuleiro(Tabuleiro),
    (
        vencedor(Tabuleiro, Consecutivas, 'O') ->
            write('Jogador 2 (O) venceu!'), nl
        ;
            empate(Tabuleiro) ->
                write('Empate!'), nl
            ;
                jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao)
    ).

jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, Peca, NovoTabuleiro) :-
    nth1(Coluna, Tabuleiro, ColunaTabuleiro),
    length(ColunaTabuleiro, AlturaColuna), % erro aqui
    AlturaColuna < Altura, % erro aqui
    inserir_peca(ColunaTabuleiro, Peca, ColunaTabuleiro1), % erro aqui
    substituir(Coluna, Tabuleiro, ColunaTabuleiro1, NovoTabuleiro), % erro aqui
    !, % erro aqui
    verificar_vitoria_horizontal(NovoTabuleiro, Consecutivas, Peca),
    verificar_vitoria_vertical(NovoTabuleiro, Consecutivas, Peca),
    verificar_vitoria_diagonal(NovoTabuleiro, Consecutivas, Peca).
    


% Insere uma peça em uma coluna
inserir_peca(ColunaTabuleiro, Peca, ColunaTabuleiro1) :-
    reverse(ColunaTabuleiro, ColunaTabuleiroInvertido),
    append([Peca], ColunaTabuleiroInvertido, ColunaTabuleiroInvertido1),
    reverse(ColunaTabuleiroInvertido1, ColunaTabuleiro1).

% Substitui uma coluna no tabuleiro
substituir(1, [_|Tabuleiro], ColunaTabuleiro1, [ColunaTabuleiro1|Tabuleiro]).
substituir(Coluna, [Tab1|Tabuleiro], ColunaTabuleiro1, [Tab1|Tabuleiro1]) :-
    Coluna > 1,
    Coluna1 is Coluna - 1,
    substituir(Coluna1, Tabuleiro, ColunaTabuleiro1, Tabuleiro1).

% Verifica se há um vencedor na horizontal
verificar_vitoria_horizontal(Tabuleiro, Consecutivas, Peca) :-
    nth1(_, Tabuleiro, Linha),
    verificar_vitoria(Linha, Consecutivas, Peca).

% Verifica se há um vencedor na vertical
verificar_vitoria_vertical(Tabuleiro, Consecutivas, Peca) :-
    transpose(Tabuleiro, TabuleiroTransposto),
    verificar_vitoria_horizontal(TabuleiroTransposto, Consecutivas, Peca).

% Verifica se há um vencedor na diagonal
verificar_vitoria_diagonal(Tabuleiro, Consecutivas, Peca) :-
    diagonal(Tabuleiro, Diagonal),
    verificar_vitoria(Diagonal, Consecutivas, Peca).

% Retorna a diagonal principal do tabuleiro
diagonal(Tabuleiro, Diagonal) :-
    diagonal(Tabuleiro, Diagonal, 1).

diagonal([], [], _).
diagonal([Linha|Tabuleiro], [Celula|Diagonal], N) :-
    nth1(N, Linha, Celula),
    N1 is N + 1,
    diagonal(Tabuleiro, Diagonal, N1).

% Verifica se há um vencedor em uma linha ou coluna
verificar_vitoria(Linha, Consecutivas, Peca) :-
    append(_, [Peca|T], Linha),
    length([Peca|T], Consecutivas),
    !.
verificar_vitoria(Linha, Consecutivas, Peca) :-
    append(T, [Peca|_], Linha),
    length(T, Consecutivas),
    !.

% Verifica se houve empate
empate(Tabuleiro) :-
    %todas_colunas_preenchidas(Tabuleiro).
    fail.

% Verifica se todas as colunas foram preenchidas
todas_colunas_preenchidas([]).
todas_colunas_preenchidas([ColunaTabuleiro|Tabuleiro]) :-
    length(ColunaTabuleiro, Altura),
    Altura > 0,
    todas_colunas_preenchidas(Tabuleiro).

% Verifica se há um vencedor
vencedor(Tabuleiro, Consecutivas, Peca) :-
    fail.
    %verificar_vitoria_horizontal(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    fail.
    %verificar_vitoria_vertical(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    fail.
    %verificar_vitoria_diagonal(Tabuleiro, Consecutivas, Peca).

:- jogar.