:- use_module(library(clpfd)).

% Predicado principal do jogo
jogar :-
    write('Escolha a versao do jogo: '), nl,
    write(1 - normal), nl,
    write(2 - simplificada), nl,
    read(Versao),

    (Versao =:= 1) ->
    write('Digite a quantidade de linhas: '),nl,
    read(Largura),

    write('Digite a quantidade de colunas: '),nl,
    read(Altura),

    write('Digite a quantidade de pecas repetidas para ganhar: '),nl,
    read(Consecutivas),

    cria_tabuleiro(Largura, Altura, Tabuleiro),
    imprime_tabuleiro(Tabuleiro),
    joga_v1(Largura, Altura, Consecutivas, Tabuleiro);

    write('Digite a quantidade de linhas: '), nl,
    read(Largura),
    write('Digite a quantidade de colunas: '), nl,
    read(Altura),
    write('Digite a quantidade de pecas repetidas p. vencer: '), nl,
    read(Consecutivas),
    cria_tabuleiro(Largura, Altura, Tabuleiro),
    imprime_tabuleiro(Tabuleiro),
    joga_v2(Largura, Altura, Consecutivas, Tabuleiro).



% PREDICADOS GERAIS DO PROBLEMA
% estabelece simbolos aceitos
simbolo('X').
simbolo('O').

% Predicado para criar tabuleiro
cria_tabuleiro(Largura, Altura, Tabuleiro) :-
    length(Tabuleiro, Largura),
    maplist(length_(Altura), Tabuleiro).
length_(Altura, Tabuleiro) :-
    length(Tabuleiro, Altura),
    maplist(nulo, Tabuleiro).
nulo('N').

% Predicado para imprimir o tabuleiro
imprime_tabuleiro(Tabuleiro) :-
    print_linhas(Tabuleiro).

% Predicados para auxiliar na imprecao do tabuleiro, linha por linha
print_linhas([]).
print_linhas([Linha|Linhas]) :-
    print_linha(Linha),
    nl,
    print_linhas(Linhas).

print_linha([]).
print_linha([Simbolo|Simbolos]) :-
    write(Simbolo),
    write(' '),
    print_linha(Simbolos).

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
joga_v1(Largura, Altura, Consecutivas, Tabuleiro) :-
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
    joga2_v1(Largura, Altura, Consecutivas, NovoTabuleiro);
    write('Posicao ja preenchida, escolha outra '), nl,
    joga_v1(Largura, Altura, Consecutivas, Tabuleiro).

% Jogador 2 joga
joga2_v1(Largura, Altura, Consecutivas, Tabuleiro) :-
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
    joga_v1(Largura, Altura, Consecutivas, NovoTabuleiro);
    write('Posicao ja preenchida, escolha outra '), nl,
    joga2_v1(Largura, Altura, Consecutivas, Tabuleiro).



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

joga_v2(Largura, Altura, Consecutivas, Tabuleiro):-
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
    joga2_v2(Largura, Altura, Consecutivas, NovoTabuleiro);
    write('Coluna selecionada esta cheia, selecione outra: '), nl,
    joga_v2(Largura, Altura, Consecutivas, Tabuleiro).

joga2_v2(Largura, Altura, Consecutivas, Tabuleiro):-
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
    joga_v2(Largura, Altura, Consecutivas, NovoTabuleiro);
    write('Coluna selecionada est a cheia, selecione outra: '), nl,
    joga2_v2(Largura, Altura, Consecutivas, Tabuleiro).


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







