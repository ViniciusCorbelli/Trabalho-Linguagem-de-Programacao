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

    criar_tabuleiro(Tabuleiro),
    imprimir_tabuleiro(Tabuleiro),
    jogador1_joga(Tabuleiro).

% Cria um tabuleiro vazio com o tamanho especificado
criar_tabuleiro(Tabuleiro) :-
    largura(Largura),
    altura(Altura),
    criar_linhas(Largura, Altura, Tabuleiro).

criar_linhas(_, 0, []).
criar_linhas(Largura, Altura, [Linha|Tabuleiro]) :-
    Altura > 0,
    length(Linha, Largura),
    Altura1 is Altura - 1,
    criar_linhas(Largura, Altura1, Tabuleiro).

% Imprime o tabuleiro
imprimir_tabuleiro(Tabuleiro) :-
    limpar_console,
    imprimir_linhas(Tabuleiro).

% Funcoes auxiliares para imprimir o tabuleiro, linha por linha
imprimir_linhas([]).
imprimir_linhas([Linha|Tabuleiro]) :-
    write(' |'),
    imprimir_linha(Linha),
    write('|'),
    nl,
    imprimir_linhas(Tabuleiro).

imprimir_linha([]).
imprimir_linha([Celula|Linha]) :-
    (nonvar(Celula), write(Celula); write(' ')),
    write(' | '),
    imprimir_linha(Linha).

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
jogador1_joga(Tabuleiro) :-
    versao(Versao),
    largura(Largura),
    altura(Altura),

    write('Vez do Jogador 1 (X)'),nl,

    (Versao =:= 1) ->
    write('Escolha uma coluna: '),nl,
    read(Coluna),
    Coluna > 0, !,
    Coluna =< Largura, !,

    write('Escolha uma linha: '), nl,
    read(Linha1),
    Linha1 > 0, !,
    Linha1 =< Altura, !,

    criar_tabuleiro(NovoTabuleiro),
    troca(Tabuleiro, Linha1, Coluna, NovoTabuleiro, 'X'),
    imprimir_tabuleiro(NovoTabuleiro),
    jogador2_joga(NovoTabuleiro)

    ;
    (Versao =:= 2) ->
    write('Versão nao implementada'), nl.

% Jogador 2 joga
jogador2_joga(Tabuleiro) :-
    versao(Versao),
     write('Vez do Jogador 2 (O)'),nl,
        
    % Verifica versão do jogo
    (Versao =:= 1) ->
    busca_astar(Tabuleiro, 'O', Linha, Coluna),
    write('Escolheu a coluna: '),write(Coluna),nl,
    write('Escolheu a linha: '),write(Linha),nl,
    
    criar_tabuleiro(NovoTabuleiro),
    troca(NovoTabuleiro, Linha, Coluna, TabuleiroMod),
    imprimir_tabuleiro(TabuleiroMod),
    jogador1_joga(TabuleiroMod)
    ;
    (Versao =:= 2) ->
    write('Versão nao implementada'), nl.
    
busca_astar(Tabuleiro, Peca, Linha, Coluna) :-
    findall((L, C), (between(1, 3, L), between(1, 3, C), nth1(L, Tabuleiro, Linha), nth1(C, Linha, e)), EspacosLivres),
    astar(Tabuleiro, Peca, EspacosLivres, (Linha, Coluna)).
        
% Cálculo da heurística h
h(Tabuleiro, Peca, (L, C), H) :-
    % Define as possíveis linhas, colunas e diagonais que podem ser formadas passando pelo espaço livre (L, C)
    nth1(L, Tabuleiro, Linha),
    transpose(Tabuleiro, Transposta),
    nth1(C, Transposta, Coluna),
    diagonal(Tabuleiro, DiagonalPrincipal),
    diagonal(reverse(Tabuleiro), DiagonalSecundaria),
    
    % Verifica quantas peças iguais consecutivas existem para cada linha, coluna e diagonal
    findall(Consecutivas, (member(Seq, [Linha, Coluna, DiagonalPrincipal, DiagonalSecundaria]), verifica_consecutivas(Seq, Peca, Consecutivas)), NumConsecutivas),
    max_list(NumConsecutivas, H).
        
% Testa se existem Consecutivas peças iguais consecutivas
verifica_consecutivas(Seq, Peca, Consecutivas) :-
    consecutivas(Consec),
    findall(Peca, nth1(_, Seq, Peca), Ocorrencias),
    one_or_more_occur(Ocorrencias, Consec, false, true),
    consecutivas == Consecutivas.
        
% Função auxiliar para verificar se um número mínimo de ocorrências de um elemento em uma lista foi atingido
one_or_more_occur(_, 1, _, true) :- !.
one_or_more_occur([], _, _, false) :- !.
one_or_more_occur([H|T], N, Prev, Found) :-
    (H == Prev ->
    M is N - 1,
    one_or_more_occur(T, M, H, Found)
    ;
    one_or_more_occur(T, N, H, Found)
    ).

% Implementação do A*
astar(Tabuleiro, Peca, EspacosLivres, (Linha, Coluna)) :-
    % Define o espaço livre com menor custo
    maplist(h(Tabuleiro, Peca), EspacosLivres, HS),
    maplist(length, EspacosLivres, GS),
    sum_list(HS, SHS),
    sum_list(GS, SGS),
    find_min([(SHS + SGS, Espaco)|_]), % Ordena pelo menor custo
    
    % Troca a peça no espaço livre e verifica se houve vitória ou empate
    troca(Tabuleiro, Linha, Coluna, NovoTabuleiro, Peca),
    (venceu(NovoTabuleiro, Peca) ->
    Linha = Linha, Coluna = Coluna
    ;
    (empate(NovoTabuleiro) ->
    Linha = Linha, Coluna = Coluna
    ;
    % Continua a busca recursivamente com o novo tabuleiro e os espaços livres restantes
    delete(EspacosLivres, (Linha, Coluna), NovosEspacosLivres),
    astar(NovoTabuleiro, Peca, NovosEspacosLivres, (L, C)),
    Linha = L, Coluna = C
    )
    ).
        
% Função auxiliar para encontrar o mínimo em uma lista de pares (Valor, Elemento)
find_min([(V, E)|T], Min) :- find_min(T, (V, E), Min).
find_min([], Min, Min).
find_min([(V, E)|T], (VM, EM), Min) :-
    (V < VM ->
    find_min(T, (V, E), Min)
    ;
    find_min(T, (VM, EM), Min)
    ).

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
    consecutivas(Consecutivas),
    append(_, [Peca|T], Linha),
    length([Peca|T],Consecutivas),
    !.
verificar_vitoria(Linha, Peca) :-
    consecutivas(Consecutivas),
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
    altura(Altura),
    length(ColunaTabuleiro, Altura),
    Altura > 0,
    todas_colunas_preenchidas(Tabuleiro).

venceu(Tabuleiro, Jogador) :-
    % verifica linhas
    member([Jogador,Jogador,Jogador], Tabuleiro),
    
    % verifica colunas
    transpose(Tabuleiro, Transposta),
    member([Jogador,Jogador,Jogador], Transposta),
    
    % verifica diagonal principal
    nth0(0, Tabuleiro, [Jogador,_,_]),
    nth0(1, Tabuleiro, [_,Jogador,_]),
    nth0(2, Tabuleiro, [_,_,Jogador]),
    
    % verifica diagonal secundária
    nth0(0, Tabuleiro, [_,_,Jogador]),
    nth0(1, Tabuleiro, [_,Jogador,_]),
    nth0(2, Tabuleiro, [Jogador,_,_]).
    

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
