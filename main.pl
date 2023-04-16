% Predicado principal do jogo
jogar :-
    write('Qual vers ̃ao do jogo: normal(1) ou simplificada(2); '),
    read(Versao),

    write('Digite a quantidade de linhas: '),
    read(Largura),

    write('Digite a quantidade de colunas: '),
    read(Altura),

    write('Digite a quantidade de peças repetidas para ganhar: '),
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
    write('   '),
    imprimir_numeros(1, Largura),
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
    write(' '),
    write(Celula),
    write(' |'),
    imprimir_linha(Linha).

imprimir_numeros(N, N).
imprimir_numeros(I, N) :-
    write(' '),
    write(I),
    I1 is I + 1,
    imprimir_numeros(I1, N).

% Jogador 1 joga
jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao) :-
    write('Jogador 1 (X), escolha uma coluna: '),
    read(Coluna),
    jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, 'X', Tabuleiro1),
    imprimir_tabuleiro(Tabuleiro1),
    (
        vencedor(Tabuleiro1, Consecutivas, 'X') ->
            write('Jogador 1 (X) venceu!'), nl
        ;
            empate(Tabuleiro1) ->
                write('Empate!'), nl
            ;
                jogador2_joga(Largura, Altura, Consecutivas, Tabuleiro1, Versao)
    ).

% Jogador 2 joga
jogador2_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao) :-
    write('Jogador 2 (O), escolha uma coluna: '),
    read(Coluna),
    jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, 'O', Tabuleiro1),
    imprimir_tabuleiro(Tabuleiro1),
    (
        vencedor(Tabuleiro1, Consecutivas, 'O') ->
            write('Jogador 2 (O) venceu!'), nl
        ;
            empate(Tabuleiro1) ->
                write('Empate!'), nl
            ;
                jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro1, Versao)
    ).

% Insere uma peça na coluna especificada
jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, Peca, Tabuleiro1) :-
    nth1(Coluna, Tabuleiro, ColunaTabuleiro),
    length(ColunaTabuleiro, AlturaColuna),
    AlturaColuna < Altura,
    inserir_peca(ColunaTabuleiro, Peca, ColunaTabuleiro1),
    substituir(Coluna, Tabuleiro, ColunaTabuleiro1, Tabuleiro1),
    !,
    verificar_vitoria_horizontal(Tabuleiro1, Consecutivas, Peca),
    verificar_vitoria_vertical(Tabuleiro1, Consecutivas, Peca),
    verificar_vitoria_diagonal(Tabuleiro1, Consecutivas, Peca).

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
    todas_colunas_preenchidas(Tabuleiro).

% Verifica se todas as colunas foram preenchidas
todas_colunas_preenchidas([]).
todas_colunas_preenchidas([ColunaTabuleiro|Tabuleiro]) :-
    length(ColunaTabuleiro, Altura),
    Altura > 0,
    todas_colunas_preenchidas(Tabuleiro).

% Verifica se há um vencedor
vencedor(Tabuleiro, Consecutivas, Peca) :-
    verificar_vitoria_horizontal(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    verificar_vitoria_vertical(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    verificar_vitoria_diagonal(Tabuleiro, Consecutivas, Peca).

:- jogar.