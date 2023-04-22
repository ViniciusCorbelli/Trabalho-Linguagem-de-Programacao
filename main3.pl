% variável global
:- dynamic versao/1.
:- dynamic largura/1.
:- dynamic altura/1.
:- dynamic consecutivas/1.

:- use_module(library(clpfd)).

solicita_versao :-
    write('Escolha a versão do jogo: '), nl,
    write('1 - Normal'), nl,
    write('2 - Simplificada'), nl,
    read(Versao),
    valida_versao(Versao).

valida_versao(Versao) :-
    Versao < 1 ->
    write('Versão inválida. Tente novamente.'), nl,
    solicita_versao
    ;

    Versao > 2 ->
    write('Versão inválida. Tente novamente.'), nl,
    solicita_versao
    ;
    % armazena o valor da versao na variável global
    asserta(versao(Versao)).

solicita_largura :-
    write('Digite a quantidade de linhas: '), nl,
    read(Largura),
    valida_largura(Largura).

valida_largura(Largura) :-
    Largura < 1 ->
    write('Largura inválida. Tente novamente.'), nl,
    solicita_largura
    ;
    % armazena o valor da largura na variável global
    asserta(largura(Largura)).

solicita_altura :-
    write('Digite a quantidade de colunas: '), nl,
    read(Altura),
    valida_altura(Altura).

valida_altura(Altura) :-
    Altura < 1 ->
    write('Altura inválida. Tente novamente.'), nl,
    solicita_altura
    ;
    % armazena o valor da altura na variável global
    asserta(altura(Altura)).

solicita_consecutivas :-
    write('Digite a quantidade de peças repetidas para ganhar: '), nl,
    read(Consecutivas),
    valida_consecutivas(Consecutivas).

valida_consecutivas(Consecutivas) :-
    % armazena o valor da consecutivas na variável global
    asserta(consecutivas(Consecutivas)).

jogar :-
    % chama o predicado que solicita a versão
    solicita_versao,

    % chama o predicado que solicita a largura
    solicita_largura,

    % chama o predicado que solicita a altura
    solicita_altura,

    % chama o predicado que solicita os repetidas
    solicita_consecutivas,

    criar_tabuleiro(Tabuleiro),
    imprimir_tabuleiro(Tabuleiro),
    jogador1_joga(Tabuleiro).

% Cria um tabuleiro vazio com o tamanho especificado
criar_tabuleiro(Tabuleiro) :-
    length(Tabuleiro, altura(Altura)),
    criar_linhas(Tabuleiro).

% Função auxiliar para criar linhas de novo tabuleiro
criar_linhas(_, 0, []).
criar_linhas([Linha|Tabuleiro]) :-
    altura(Altura) > 0,
    length(Linha, largura(Largura)),
    Altura1 is altura(Altura) - 1,
    criar_linhas(Altura1, Tabuleiro).

% Função para copiar um tabuleiro
copia_tabuleiro(Tabuleiro, NovoTabuleiro) :-
    copy_term(Tabuleiro, NovoTabuleiro).

% Imprime o tabuleiro
imprimir_tabuleiro(Tabuleiro) :-
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

% função para substituir um elemento em uma posição específica
troca([Linha|Linhas], 1, Coluna, [NovaLinha|Linhas]) :-
    troca_linha(Linha, Coluna, 'X', NovaLinha).
troca([Linha|Linhas], NumLinha, Coluna, [Linha|NovasLinhas]) :-
    NumLinha > 1,
    NumLinha1 is NumLinha - 1,
    troca(Linhas, NumLinha1, Coluna, NovasLinhas).

% função para substituir um elemento em uma linha específica
troca_linha([_|Colunas], 1, X, [X|Colunas]).
troca_linha([Coluna|Colunas], NumColuna, X, [Coluna|NovasColunas]) :-
    NumColuna > 1,
    NumColuna1 is NumColuna - 1,
    troca_linha(Colunas, NumColuna1, X, NovasColunas).

% Jogador 1 joga
jogador1_joga(Tabuleiro) :-
    write('Jogador 1 (X), escolha uma coluna: '),nl,
    read(Coluna),

    % Verifica se a coluna é válida
    (Coluna < 1 ; Coluna > largura(Largura)) ->
    write('Coluna invalida. Tente novamente.'), nl,
    jogador1_joga(Tabuleiro)
    ;

    % Verifica versão do jogo
    (versao(Versao) =:= 1) ->
    write('Jogador 1 (X), escolha uma linha: '), nl,
    read(Linha),
    criar_tabuleiro(NovoTabuleiro),
    copia_tabuleiro(Tabuleiro, NovoTabuleiro),
    troca(NovoTabuleiro, Linha, Coluna, TabuleiroMod),
    imprimir_tabuleiro(TabuleiroMod),
    jogador2_joga(TabuleiroMod)
    ;
    (versao(Versao) =:= 2) ->
    write('versao(Versao) nao implementada'), nl.

% Jogador 2 joga
jogador2_joga(Tabuleiro) :-
    write('Vez do Jogador 2 (O)'), nl,
    jogada_minimax(Tabuleiro, 'O', Jogada),
    [Linha, Coluna] = Jogada,
    criar_tabuleiro(NovoTabuleiro),
    copia_tabuleiro(Tabuleiro, NovoTabuleiro),
    troca(NovoTabuleiro, Linha, Coluna, TabuleiroMod),
    jogar(Tabuleiro, Coluna, Linha, 'O', NovoTabuleiro),
    jogador1_joga(TabuleiroMod).

jogada_minimax(Tabuleiro, Jogador, NovoTabuleiro) :-
    findall(
        [V, NovoTabuleiro1],
        (between(1, largura(Largura), coluna(Coluna)),
            jogada_possivel(Tabuleiro, Coluna),
            jogar(Tabuleiro, Coluna, _, Jogador, NovoTabuleiro1),
            minimax(NovoTabuleiro1, 'X', 1, _, V)),
        ListaJogadas),
    seleciona_melhor_jogada(ListaJogadas, _, NovoTabuleiro).

% Seleciona a melhor jogada para o jogador
seleciona_melhor_jogada(Tabuleiro, Jogador, MelhorJogada) :-
    nth1(1, Tabuleiro, PrimeiraLinha), 
    length(PrimeiraLinha, Altura),
    length(Posicoes, Altura),
    domain(Posicoes, 1,  largura(Largura)),
    verifica_jogadas(Tabuleiro, Jogador, Posicoes),
    labeling([max], Posicoes),
    nth1(Altura, Posicoes, MelhorJogada).
    
% Verifica todas as jogadas possíveis para o jogador
verifica_jogadas(_, _, _, []).
verifica_jogadas(Tabuleiro, Jogador, [Jogada|Resto]) :-
    jogar_pecas(Jogador, Jogada, Tabuleiro, NovoTabuleiro),
    not(vencedor(NovoTabuleiro, consecutivas(Consecutivas), Jogador)),
    verifica_jogadas(Tabuleiro, Jogador, Resto).
verifica_jogadas(Tabuleiro, Jogador, [Jogada|_]) :-
    jogar_pecas(Jogador, Jogada, Tabuleiro, NovoTabuleiro),
    vencedor(NovoTabuleiro, Jogador).


% Verifica se há um vencedor na horizontal
verificar_vitoria_horizontal(Tabuleiro, Peca) :-
    nth1(_, Tabuleiro, Linha),
    verificar_vitoria(Linha, Peca).

% Verifica se há um vencedor na vertical
verificar_vitoria_vertical(Tabuleiro, Peca) :-
    transpose(Tabuleiro, TabuleiroTransposto),
    verificar_vitoria_horizontal(TabuleiroTransposto, Peca).

% Verifica se há um vencedor na diagonal
verificar_vitoria_diagonal(Tabuleiro, Peca) :-
    diagonal(Tabuleiro, Diagonal),
    verificar_vitoria(Diagonal, Peca).

% Retorna a diagonal principal do tabuleiro
diagonal(Tabuleiro, Diagonal) :-
    diagonal(Tabuleiro, Diagonal, 1).

diagonal([], [], _).
diagonal([Linha|Tabuleiro], [Celula|Diagonal], N) :-
    nth1(N, Linha, Celula),
    N1 is N + 1,
    diagonal(Tabuleiro, Diagonal, N1).

% Verifica se há um vencedor em uma linha ou coluna
verificar_vitoria(Linha, Peca) :-
    append(_, [Peca|T], Linha),
    length([Peca|T], consecutivas(Consecutivas)),
    !.
verificar_vitoria(Linha, Peca) :-
    append(T, [Peca|_], Linha),
    length(T, consecutivas(Consecutivas)),
    !.

% Verifica se houve empate
empate(Tabuleiro) :-
    %todas_colunas_preenchidas(Tabuleiro).
    fail.

% Verifica se todas as colunas foram preenchidas
todas_colunas_preenchidas([]).
todas_colunas_preenchidas([ColunaTabuleiro|Tabuleiro]) :-
    length(ColunaTabuleiro, altura(Altura)),
    altura(Altura) > 0,
    todas_colunas_preenchidas(Tabuleiro).

% Verifica se há um vencedor
vencedor(Tabuleiro, Peca) :-
    fail.
    %verificar_vitoria_horizontal(Tabuleiro, Peca).
vencedor(Tabuleiro, Peca) :-
    fail.
    %verificar_vitoria_vertical(Tabuleiro, Peca).
vencedor(Tabuleiro, Peca) :-
    fail.
    %verificar_vitoria_diagonal(Tabuleiro, Peca).
