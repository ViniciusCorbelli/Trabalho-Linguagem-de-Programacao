% variável global
:- dynamic versao/1.
:- dynamic altura/1.
:- dynamic largura/1.
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

solicita_altura :-
    write('Digite a quantidade de linhas: '), nl,
    read(Altura),
    valida_altura(Altura).

valida_altura(Altura) :-
    Altura < 1 ->
    write('Altura inválida. Tente novamente.'), nl,
    solicita_altura
    ;
    % armazena o valor da altura na variável global
    asserta(altura(Altura)),
    Largura is Altura + 1,
    asserta(largura(Largura)).

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
    altura(Altura),
    largura(Largura),

    length(Tabuleiro, Altura),
    maplist(length_(Largura), Tabuleiro).
length_(Largura, Tabuleiro) :-
    length(Tabuleiro, Largura),
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
troca([Linha|Linhas], 1, Coluna, [NovaLinha|Linhas], Simbolo) :-
    troca_linha(Linha, Coluna, Simbolo, NovaLinha).
troca([Linha|Linhas], NumLinha, Coluna, [Linha|NovasLinhas], Simbolo) :-
    NumLinha > 1,
    NumLinha1 is NumLinha - 1,
    troca(Linhas, NumLinha1, Coluna, NovasLinhas, Simbolo).

% Predicado para substituir um elemento em uma linha específica
troca_linha([_|Colunas], 1, Simbolo, [Simbolo|Colunas]).
troca_linha([Coluna|Colunas], NumColuna, Simbolo, [Coluna|NovasColunas]) :-
    NumColuna =\= 1,
    NumColuna1 is NumColuna - 1,
    troca_linha(Colunas, NumColuna1, Simbolo, NovasColunas).

% Predicado para fazer jogada
joga_v1(Tabuleiro) :-
    altura(Altura),
    largura(Largura),
    consecutivas(Consecutivas),

    write('Escolha uma coluna: '),nl,
    read(Coluna),

    Coluna > 0, !,
    Coluna =< Altura, !,

    write('Escolha uma linha: '), nl,
    read(Linha),

    Linha > 0, !,
    Linha =< Largura, !,

    (verifica_simbolo(Tabuleiro, Linha, Coluna)) ->
    troca(Tabuleiro, Linha, Coluna, NovoTabuleiro, 'X'),
    imprime_tabuleiro(NovoTabuleiro),
    not(verifica_vitoria(NovoTabuleiro, 'X', 2, Coluna, Linha)),
    joga2_v1(NovoTabuleiro);
    write('Posicao ja preenchida, escolha outra '), nl,
    joga_v1(Tabuleiro).

% Jogador 2 joga
joga2_v1(Tabuleiro) :-
    altura(Altura),
    largura(Largura),
    consecutivas(Consecutivas),
    
    %% Encontra a primeira posição vazia em que o jogador 2 pode jogar e vencer
    possivel_vitoria(Tabuleiro, Linha, Coluna),

    (verifica_simbolo(Tabuleiro, Linha, Coluna)) ->
    troca(Tabuleiro, Linha, Coluna, NovoTabuleiro, 'O'),
    imprime_tabuleiro(NovoTabuleiro),
    not(verifica_vitoria(NovoTabuleiro, 'O', 2, Coluna, Linha)),
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
    troca(Tabuleiro, Indice, Coluna, NovoTabuleiro, Simbolo).


verifica_coluna(Tabuleiro, Coluna) :-
    transpose(Tabuleiro, Transposta),
    Coluna1 is Coluna - 1,
    nth0(Coluna1, Transposta, ColunaEscolhida),
    member(Col, ColunaEscolhida),
    Col = 'N'.

joga_v2(Tabuleiro):-
    altura(Altura),

    write('Escolha uma coluna: '), nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Altura, !,

    (verifica_coluna(Tabuleiro, Coluna)) ->
    troca_v2(Tabuleiro, Coluna, 'X', NovoTabuleiro, Indice),
    imprime_tabuleiro(NovoTabuleiro),
    not(verifica_vitoria(NovoTabuleiro, 'X', 1, Coluna, Altura)),
    joga2_v2(NovoTabuleiro);
    write('Coluna selecionada esta cheia, selecione outra: '), nl,
    joga_v2(Tabuleiro).

joga2_v2(Tabuleiro):-
    altura(Altura),
    largura(Largura),

    %% Encontra a primeira posição vazia em que o jogador 2 pode jogar e vencer
    possivel_vitoria(Tabuleiro, Linha, Coluna),

    (verifica_simbolo(Tabuleiro, Linha, Coluna)) ->
    troca_v2(Tabuleiro, Coluna, NovoTabuleiro, 'O'),
    imprime_tabuleiro(NovoTabuleiro),
    not(verifica_vitoria(NovoTabuleiro, 'O', 2, Coluna, Linha)),
    joga_v2(NovoTabuleiro);
    write('Coluna selecionada esta cheia, selecione outra: '), nl,
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

% Verifica vencedor nas diagonais
vitoria_diagonal(Tabuleiro, Simbolo, Jogador, Largura):-
    pega_diagonal1(Tabuleiro, 1, Diagonal1),
    pega_diagonal1(Tabuleiro, 2, Diagonal2),
    pega_diagonal2(Tabuleiro, Largura, Diagonal3),
    Largura1 is Largura - 1,
    pega_diagonal2(Tabuleiro, Largura1, Diagonal4),
    (mesmo_simbolo(Diagonal1, Simbolo); mesmo_simbolo(Diagonal2, Simbolo); mesmo_simbolo(Diagonal3, Simbolo) ; mesmo_simbolo(Diagonal4, Simbolo)) ->
    write('Vitoria do jogador: '), write(Jogador), nl,
    jogar;
    fail.

% Predicado que pega a diagonal principal mais a diagonal que comeca em
% coluna + 1.
pega_diagonal1(Tabuleiro, Indice, Diagonal) :-
    pega_diagonal_aux1(Tabuleiro, Indice, Diagonal).

pega_diagonal_aux1([], _, []).
pega_diagonal_aux1([Linha|OutrasLinhas], Indice, [Elemento|DiagonalRestante]) :-
    nth1(Indice, Linha, Elemento),
    NovoIndice is Indice + 1,
    pega_diagonal_aux1(OutrasLinhas, NovoIndice, DiagonalRestante).

% Predicado que pega a diagonal secund�ria mais a diagonal que come�a em
% coluna - 1.
pega_diagonal2(Tabuleiro, Indice, Diagonal) :-
    pega_diagonal_aux2(Tabuleiro, Indice, Diagonal).

pega_diagonal_aux2([], _, []).
pega_diagonal_aux2([Linha|OutrasLinhas], Indice, [Elemento|DiagonalRestante]) :-
    nth1(Indice, Linha, Elemento),
    NovoIndice is Indice - 1,
    pega_diagonal_aux2(OutrasLinhas, NovoIndice, DiagonalRestante).

% Verifica vencedor
verifica_vitoria(Tabuleiro, Simbolo, Jogador, Coluna, Linha) :-
    vitoria_vertical(Tabuleiro, Coluna, Simbolo, Jogador),
    Vencedor = Simbolo.
    
verifica_vitoria(Tabuleiro, Simbolo, Vencedor, Coluna, Linha) :-
    vitoria_horizontal(Tabuleiro, Linha, Simbolo, Jogador),
    Vencedor = Simbolo.
    
verifica_vitoria(Tabuleiro, Simbolo, Vencedor, Coluna, Linha) :-
    vitoria_diagonal(Tabuleiro, Simbolo, Jogador, Linha),
    Vencedor = Simbolo.

/*verifica_vitoria(Tabuleiro, Simbolo, Vencedor, Coluna, Linha) :-
    empate(Tabuleiro),
    Vencedor = Simbolo.*/
    
% Verifica se empatou
empate(Tabuleiro) :-
    write('Jogo empatou'), nl,
    member('N', Tabuleiro).

possivel_vitoria(Tabuleiro, Linha, Coluna) :-
    altura(Altura),
    between(1, Altura, Linha),
    nth1(Linha, Tabuleiro, LinhaTabuleiro),
    nth1(Coluna, LinhaTabuleiro, 'N').

possivel_vitoria(Tabuleiro, Linha, Coluna) :-
    altura(Altura),
    consecutivas(Consecutivas),
    % verifica todas as linhas
    between(1, Altura, Linha),
    nth1(Linha, Tabuleiro, LinhaTabuleiro),
    % procura sequencias de peças do mesmo jogador
    append(, [Simbolo|SeqRestante], LinhaTabuleiro),
    same(Simbolo, SeqRestante, Consecutivas),
    % procura posição vazia após a sequencia
    append(, [Simbolo|SeqFinal], LinhaTabuleiro),
    length(SeqFinal, RestoSeq),
    RestoSeq >= Consecutivas,
    append(Prefixo, [Posicao|_], SeqFinal),
    length(Prefixo, Consecutivas),
    Posicao = ‘N’,
    % encontra coluna correspondente
    nth1(Coluna, LinhaTabuleiro, ‘N’).

:- jogar.