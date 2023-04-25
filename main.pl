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
    largura(Largura),
    altura(Altura),

    write('Vez do Jogador 2 (O)'),nl,

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
    troca(Tabuleiro, Linha1, Coluna, NovoTabuleiro, 'O'),
    imprimir_tabuleiro(NovoTabuleiro),
    jogador1_joga(NovoTabuleiro)

    ;
    (Versao =:= 2) ->
    write('Versão nao implementada'), nl.

melhor_jogada(Tabuleiro, Jogador, MelhorJogada) :-
    findall(Pos, posicao_vazia(Tabuleiro, Pos), PosicoesVazias),
    minimax(Tabuleiro, Jogador, PosicoesVazias, [], _, MelhorJogada, _).
    
% avalia a melhor jogada utilizando o algoritmo Minimax
minimax(Tabuleiro, _, [], _, Pontuacao, _, _) :-
    avalia_tabuleiro(Tabuleiro, Pontuacao).
minimax(Tabuleiro, Jogador, [Posicao|Posicoes], Caminho, MelhorPontuacao, MelhorJogada, Profundidade) :-
    profundidade_maxima(ProfundidadeMaxima),
    Profundidade < ProfundidadeMaxima,
    copia_tabuleiro(Tabuleiro, NovoTabuleiro),
    troca(NovoTabuleiro, Posicao, Jogador, NovoTabuleiro2),
    outro_jogador(Jogador, OutroJogador),
    proxima_jogada(OutroJogador, ProximaJogada),
    NovaProfundidade is Profundidade + 1,
    minimax(NovoTabuleiro2, ProximaJogada, Posicoes, [Posicao|Caminho], Pontuacao, _, NovaProfundidade),
    melhor_jogada(Jogador, Caminho, MelhorJogada, Pontuacao, MelhorPontuacao, Posicao, Posicoes).
    
% encontra a melhor jogada para o jogador 2
melhor_jogada('O', [], MelhorJogada, _, _, MelhorJogada, _).
melhor_jogada('O', [Jogada|Caminho], MelhorJogadaAtual, PontuacaoAtual, MelhorPontuacao, _, _) :-
    avalia_jogada('O', Jogada, Pontuacao),
    (Pontuacao > PontuacaoAtual ->
        melhor_jogada('O', Caminho, Jogada, Pontuacao, MelhorPontuacao, _, _)
    ;
        melhor_jogada('O', Caminho, MelhorJogadaAtual, PontuacaoAtual, MelhorPontuacao, _, _)
    ).
    
% encontra a melhor jogada para o jogador 1
melhor_jogada('X', [], MelhorJogada, _, _, MelhorJogada, _).
melhor_jogada('X', [Jogada|Caminho], MelhorJogadaAtual, PontuacaoAtual, MelhorPontuacao, _, _) :-
    avalia_jogada('X', Jogada, Pontuacao),
    (Pontuacao < PontuacaoAtual ->
        melhor_jogada('X', Caminho, Jogada, Pontuacao, MelhorPontuacao, _, _)
    ;
        melhor_jogada('X', Caminho, MelhorJogadaAtual, PontuacaoAtual, MelhorPontuacao, _, _)
    ).

avalia_tabuleiro(Tabuleiro, Pontuacao) :-
    % Verifica se algum jogador venceu
    venceu(Tabuleiro, 'X'), Pontuacao is -100;
    venceu(Tabuleiro, 'O'), Pontuacao is 100;
    
    % Verifica se o jogo empatou
    jogo_empatado(Tabuleiro), Pontuacao is 0;
    
    % Caso contrário, calcula a pontuação baseada na quantidade de peças em jogo
    % do jogador e do adversário
    count_ocorrencias(Tabuleiro, 'O', QtdO),
    count_ocorrencias(Tabuleiro, 'X', QtdX),
    Pontuacao is QtdO - QtdX.

count_ocorrencias(Tabuleiro, Simbolo, Qtd) :-
    flatten(Tabuleiro, TabuleiroAchatado),
    include(=(Simbolo), TabuleiroAchatado, Ocorrencias),
    length(Ocorrencias, Qtd).
    

% Retorna a posição vazia (com valor ' ') em uma coluna específica
posicao_vazia(Tabuleiro, Posicao) :-
    nth0(Linha, Tabuleiro, LinhaTabuleiro),
    nth0(Coluna, LinhaTabuleiro, Vazio),
    Vazio == ' ',
    Posicao = [Linha, Coluna].

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
