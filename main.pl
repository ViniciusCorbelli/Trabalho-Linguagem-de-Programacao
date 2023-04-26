% variável global
:- dynamic versao/1.
:- dynamic largura/1.
:- dynamic altura/1.
:- dynamic consecutivas/1.

:- use_module(library(clpfd)).

limpar_console :-
    (current_prolog_flag(windows, true) ->
        shell('cls') % se for Windows, executa o comando "cls"
    ;
        shell('clear') % caso contrário, executa o comando "clear"
    ).

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


% Predicado principal do jogo
jogar :-
    limpar_console,
    % chama o predicado que solicita a versão
    solicita_versao,

    % chama o predicado que solicita a largura
    solicita_largura,

    % chama o predicado que solicita a altura
    solicita_altura,

    % chama o predicado que solicita os repetidas
    solicita_consecutivas,

    cria_tabuleiro(Tabuleiro),
    imprime_tabuleiro(Tabuleiro),

    versao(Versao),

    (Versao =:= 1) ->
    joga_v1(Tabuleiro);

    joga_v2(Tabuleiro).

% PREDICADOS GERAIS DO PROBLEMA
% estabelece simbolos aceitos
simbolo('X').
simbolo('O').

% Predicado para criar tabuleiro
cria_tabuleiro(Tabuleiro) :-
    largura(Largura),
    altura(Altura),

    length(Tabuleiro, Largura),
    maplist(length_(Altura), Tabuleiro).
length_(Altura, Tabuleiro) :-
    length(Tabuleiro, Altura),
    maplist(nulo, Tabuleiro).
nulo('N').

% Predicado para imprimir o tabuleiro
imprime_tabuleiro(Tabuleiro) :-
    limpar_console,
    imprimir_linhas(Tabuleiro).

% Predicados para auxiliar na imprecao do tabuleiro, linha por linha
imprimir_linhas([]).
imprimir_linhas([Linha|Tabuleiro]) :-
    write(' |'),
    imprimir_linha(Linha),
    write('|'),
    nl,
    imprimir_linhas(Tabuleiro).

imprimir_linha([]).
imprimir_linha([Celula|Linha]) :-
    write(Celula),
    write(' | '),
    imprimir_linha(Linha).

% Predicado que tenta buscar simbolo na posicao m x n do tabuleiro
verifica_simbolo(Tabuleiro, Linha, Coluna):-
    nth1(Linha, Tabuleiro, LinhaTabuleiro),
    nth1(Coluna, LinhaTabuleiro, PosSim),
    not(simbolo(PosSim)).

% PREDICADOS PARA A VERSAO 1 DO JOGO DA VELHA
% Predicado para substituir um elemento em uma posição específica
troca_v1([Linha|Linhas], 1, Coluna, [NovaLinha|Linhas], Simbolo) :-
    troca_linha(Linha, Coluna, Simbolo, NovaLinha).
troca_v1([Linha|Linhas], NumLinha, Coluna, [Linha|NovasLinhas], Simbolo) :-
    NumLinha > 1,
    NumLinha1 is NumLinha - 1,
    troca_v1(Linhas, NumLinha1, Coluna, NovasLinhas, Simbolo).

% Predicado para substituir um elemento em uma linha específica
troca_linha([_|Colunas], 1, Simbolo, [Simbolo|Colunas]).
troca_linha([Coluna|Colunas], NumColuna, Simbolo, [Coluna|NovasColunas]) :-
    NumColuna =\= 1,
    NumColuna1 is NumColuna - 1,
    troca_linha(Colunas, NumColuna1, Simbolo, NovasColunas).

% Predicado para fazer jogada
joga_v1(Tabuleiro) :-
    largura(Largura),
    altura(Altura),
    consecutivas(Consecutivas),

    write('Jogador 1 (X), escolha uma coluna: '),nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Largura, !,
    write('Jogador 1 (X), escolha uma linha: '), nl,
    read(Linha1),
    Linha1 > 0, !,
    Linha1 =< Altura, !,
    (verifica_simbolo(Tabuleiro, Linha1, Coluna)) ->
    troca_v1(Tabuleiro, Linha1, Coluna, NovoTabuleiro, 'X'),
    imprime_tabuleiro(NovoTabuleiro),
    not(vitoria_horizontal(NovoTabuleiro, Linha1, 'X', 1)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'X', 1)),
    joga2_v1(NovoTabuleiro);
    write('Posicao ja preenchida, escolha outra '), nl,
    joga_v1(Tabuleiro).

% Jogador 2 joga
joga2_v1(Tabuleiro) :-
    largura(Largura),
    altura(Altura),

    write('Jogador 2 (O), escolha uma coluna: '),nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Largura, !,
    write('Jogador 2 (O), escolha uma linha: '), nl,
    read(Linha1),
    Linha1 > 0, !,
    Linha1 =< Altura, !,
    (verifica_simbolo(Tabuleiro, Linha1, Coluna)) ->
    troca_v1(Tabuleiro, Linha1, Coluna, NovoTabuleiro, 'O'),
    imprime_tabuleiro(NovoTabuleiro),
    not(vitoria_horizontal(NovoTabuleiro, Linha1, 'O', 2)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'O', 2)),
    joga_v1(NovoTabuleiro);
    write('Posicao ja preenchida, escolha outra '), nl,
    joga2_v1(Tabuleiro).



% PREDICADOS PARA A VERSAO 2

procura_indice(Tabuleiro, Coluna, Indice) :-
    nth0(Coluna, Tabuleiro, ColunaEscolhida),
    reverse(ColunaEscolhida, ColunaInvertida),
    length(ColunaInvertida, Tam),
    nth0(Aux_Indice, ColunaInvertida, 'N'),
    Indice is Tam - Aux_Indice.

troca_v2(Tabuleiro, Coluna, Simbolo, NovoTabuleiro, Indice) :-
    Coluna1 is Coluna - 1,
    transpose(Tabuleiro, Transposta),
    procura_indice(Transposta, Coluna1, Indice),
    troca_v1(Tabuleiro, Indice, Coluna, NovoTabuleiro, Simbolo).


verifica_coluna(Tabuleiro, Coluna) :-
    transpose(Tabuleiro, Transposta),
    Coluna1 is Coluna - 1,
    nth0(Coluna1, Transposta, ColunaEscolhida),
    member(Col, ColunaEscolhida),
    Col = 'N'.

joga_v2(Tabuleiro):-
    largura(Largura),

    write('Jogador 1 (X), escolha uma coluna: '), nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Largura, !,
    (verifica_coluna(Tabuleiro, Coluna)) ->
    troca_v2(Tabuleiro, Coluna, 'X', NovoTabuleiro, Indice),
    imprime_tabuleiro(NovoTabuleiro),
    not(vitoria_horizontal(NovoTabuleiro, Indice, 'X', 1)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'X', 1)),
    %empate(NovoTabuleiro),
    joga2_v2(NovoTabuleiro);
    write('Coluna selecionada esta cheia, selecione outra: '), nl,
    joga_v2(Tabuleiro).

joga2_v2(Tabuleiro):-
    largura(Largura),

    write('Jogador 2 (O), escolha uma coluna: '), nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Largura, !,
    (verifica_coluna(Tabuleiro, Coluna)) ->
    troca_v2(Tabuleiro, Coluna, 'O', NovoTabuleiro, Indice),
    imprime_tabuleiro(NovoTabuleiro),
    not(vitoria_horizontal(NovoTabuleiro, Indice, 'O', 2)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'O', 2)),
    %empate(NovoTabuleiro),
    joga_v2(NovoTabuleiro);
    write('Coluna selecionada est a cheia, selecione outra: '), nl,
    joga2_v2(Tabuleiro).


% PREDICADOS PARA DETERMINAR VENCEDOR

% Verifica vencedor na horizontal
vitoria_horizontal(Tabuleiro, Linha, Simbolo, Jogador) :-
    Linha1 is Linha - 1,
    nth0(Linha1, Tabuleiro, LinhaTeste),
    mesmo_simbolo(LinhaTeste, Simbolo),
    write('Vitoria do jogador: '), write(Jogador), nl,
    write('Digite 1 para nova partida: '), nl,
    read(NovaPartida),
    (NovaPartida == 1)->
    jogar.

mesmo_simbolo([],_).
mesmo_simbolo([H|T], H) :- mesmo_simbolo(T,H).

% Verifica vencedor na vertical
vitoria_vertical(Tabuleiro, Coluna, Simbolo, Jogador) :-
    Coluna1 is Coluna - 1,
    transpose(Tabuleiro, Transposta),
    nth0(Coluna1, Transposta, LinhaTeste),
    mesmo_simbolo(LinhaTeste, Simbolo),
    write('Vitoria do jogador: '), write(Jogador), nl,
    write('Digite 1 para nova partida: '), nl,
    read(NovaPartida),
    (NovaPartida == 1) ->
    jogar.

% Verifica se empatou
empate(Tabuleiro) :-
    member('N', Tabuleiro).

%:- jogar.







