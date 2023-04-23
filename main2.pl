:- use_module(library(clpfd)).

% Predicado principal do jogo
jogar :-
    write('Escolha a versÃ£o do jogo: '),nl,
    write(1 - normal),nl,
    write(2 - simplificada),nl,
    read(Versao),

    write('Digite a quantidade de linhas: '),nl,
    read(Largura),

    write('Digite a quantidade de colunas: '),nl,
    read(Altura),

    write('Digite a quantidade de peÃ§as repetidas para ganhar: '),nl,
    read(Consecutivas),

    criar_tabuleiro(Largura, Altura, Tabuleiro),
    imprime_tabuleiro(Tabuleiro),
    jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao).

% Cria um tabuleiro vazio com o tamanho especificado
criar_tabuleiro(Largura, Altura, Tabuleiro) :-
    criar_linhas(Largura, Altura, Tabuleiro).

% Funcao auxiliar para criar linhas de novo tabuleiro
criar_linhas(_, 0, []).
criar_linhas(Largura, Altura, [Linha|Tabuleiro]) :-
    Altura > 0,
    length(Linha, Largura),
    Altura1 is Altura - 1,
    criar_linhas(Largura, Altura1, Tabuleiro).

% Funcao para imprimir o tabuleiro
imprime_tabuleiro(Tabuleiro) :-
    print_linhas(Tabuleiro).

% Funcoes auxiliares para imprimir o tabuleiro, linha por linha
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

% função para substituir um elemento em uma posição específica
troca([Linha|Linhas], 1, Coluna, [NovaLinha|Linhas], Simbolo) :-
    troca_linha(Linha, Coluna, Simbolo, NovaLinha).
troca([Linha|Linhas], NumLinha, Coluna, [Linha|NovasLinhas], Simbolo) :-
    NumLinha > 1,
    NumLinha1 is NumLinha - 1,
    troca(Linhas, NumLinha1, Coluna, NovasLinhas, Simbolo).

% função para substituir um elemento em uma linha específica
troca_linha([_|Colunas], 1, Simbolo, [Simbolo|Colunas]).
troca_linha([Coluna|Colunas], NumColuna, Simbolo, [Coluna|NovasColunas]) :-
    NumColuna =\= 1,
    NumColuna1 is NumColuna - 1,
    troca_linha(Colunas, NumColuna1, Simbolo, NovasColunas).

% Jogador 1 joga
jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao) :-
    (Versao =:= 1) ->
    write('Jogador 1 (X), escolha uma coluna: '),nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Largura, !,
    write('Jogador 1 (X), escolha uma linha: '), nl,
    read(Linha1),
    Linha1 > 0, !,
    Linha1 =< Altura, !,
    criar_tabuleiro(Largura, Altura, NovoTabuleiro),
    troca(Tabuleiro, Linha1, Coluna, NovoTabuleiro, 'X'),
    imprime_tabuleiro(NovoTabuleiro),
    jogador2_joga(Largura, Altura, Consecutivas, NovoTabuleiro, Versao);
    write('Versao 2 ainda nao implementada '),
    fail.

% Jogador 2 joga
jogador2_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao) :-
    (Versao =:= 1) ->
    write('Jogador 2 (O), escolha uma coluna: '),nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Largura, !,
    write('Jogador 2 (O), escolha uma linha: '), nl,
    read(Linha1),
    Linha1 > 0, !,
    Linha1 =< Altura, !,
    criar_tabuleiro(Largura, Altura, NovoTabuleiro),
    troca(Tabuleiro, Linha1, Coluna, NovoTabuleiro, 'O'),
    imprime_tabuleiro(NovoTabuleiro),
    jogador1_joga(Largura, Altura, Consecutivas, NovoTabuleiro, Versao);
    write('Versao 2 ainda nao implementada'),
    fail.


% Verifica se hÃ¡ um vencedor na horizontal
verificar_vitoria_horizontal(Tabuleiro, Consecutivas, Peca) :-
    nth1(_, Tabuleiro, Linha),
    verificar_vitoria(Linha, Consecutivas, Peca).

% Verifica se hÃ¡ um vencedor na vertical
verificar_vitoria_vertical(Tabuleiro, Consecutivas, Peca) :-
    transpose(Tabuleiro, TabuleiroTransposto),
    verificar_vitoria_horizontal(TabuleiroTransposto, Consecutivas, Peca).

% Verifica se hÃ¡ um vencedor na diagonal
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

% Verifica se hÃ¡ um vencedor em uma linha ou coluna
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

% Verifica se hÃ¡ um vencedor
vencedor(Tabuleiro, Consecutivas, Peca) :-
    fail.
    %verificar_vitoria_horizontal(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    fail.
    %verificar_vitoria_vertical(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    fail.
    %verificar_vitoria_diagonal(Tabuleiro, Consecutivas, Peca).

%:- jogar.
