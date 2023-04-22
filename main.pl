:- use_module(library(clpfd)).

% Predicado principal do jogo
jogar :-
    write('Escolha a versão do jogo: '),nl,
    write('1 - Normal'),nl,
    write('2 - Simplificada'),nl,
    read(Versao),

    % Verifica se a versão é válida
    (Coluna < 1 ; Coluna > 2) ->
    write('Versão inválida. Tente novamente.'), nl,
    jogar
    ;

    write('Digite a quantidade de linhas: '),nl,
    read(Largura),

    % Verifica se a largura é válida
    (Largura < 1) ->
    write('Largura inválida. Tente novamente.'), nl,
    jogar
    ;

    write('Digite a quantidade de colunas: '),nl,
    read(Altura),

    % Verifica se a altura é válida
    (Altura < 1) ->
    write('Altura inválida. Tente novamente.'), nl,
    jogar
    ;

    write('Digite a quantidade de peças repetidas para ganhar: '),nl,
    read(Consecutivas),

     % Verifica se a consecutivas é válida
     (Consecutivas > Altura; Consecutivas > Largura) ->
     write('Consecutivas inválida. Tente novamente.'), nl,
     jogar
     ;

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
    write('Vez do Jogador 1 (X)'),nl,

    % Coluna
    write('Escolha a posição da coluna: '),nl,
    read(Coluna),

    % Verifica se a coluna é válida
    (Coluna < 1 ; Coluna > Largura) ->
    write('Coluna inválida. Tente novamente.'), nl,
    jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao)
    ;

    % Linha
    write('Escolha a posição da linha: '),nl,
    read(Linha),

    % Verifica se a Linha é válida
    (Linha < 1 ; Linha > Altura) ->
    write('Linha inválida. Tente novamente.'), nl,
    jogador1_joga(Largura, Altura, Consecutivas, Tabuleiro, Versao)
    ;

    criar_tabuleiro(Largura, Altura, NovoTabuleiro),
    jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, Linha, 'X', Versao, NovoTabuleiro),
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
    write('Vez do Jogador 2 (O)'), nl,
    jogada_minimax(Largura, Altura, Consecutivas, Tabuleiro, 'O', Jogada),
    [Linha, Coluna] = Jogada,
    criar_tabuleiro(Largura, Altura, NovoTabuleiro),
    jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, Linha, 'O', Versao, NovoTabuleiro),
    imprimir_tabuleiro(NovoTabuleiro),
    (
        vencedor(NovoTabuleiro, Consecutivas, 'O') ->
            write('Jogador 2 (O) venceu!'), nl
        ;
            empate(NovoTabuleiro) ->
                write('Empate!'), nl
            ;
                jogador1_joga(Largura, Altura, Consecutivas, NovoTabuleiro, Versao)
    ).

jogada_minimax(Largura, Altura, Consecutivas, Tabuleiro, Jogador, NovoTabuleiro) :-
    findall(
        [V, NovoTabuleiro1],
        (between(1, Largura, Coluna),
            jogada_possivel(Tabuleiro, Coluna),
            jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, _, Jogador, NovoTabuleiro1),
            minimax(NovoTabuleiro1, Consecutivas, 'X', 1, _, V)),
        ListaJogadas),
    seleciona_melhor_jogada(ListaJogadas, _, NovoTabuleiro).

% Seleciona a melhor jogada para o jogador
seleciona_melhor_jogada(Tabuleiro, Jogador, Consecutivas, Largura, MelhorJogada) :-
    nth1(1, Tabuleiro, PrimeiraLinha), 
    length(PrimeiraLinha, Altura),
    length(Posicoes, Altura),
    domain(Posicoes, 1, Largura),
    verifica_jogadas(Tabuleiro, Jogador, Consecutivas, Posicoes),
    labeling([max], Posicoes),
    nth1(Altura, Posicoes, MelhorJogada).
    
% Verifica todas as jogadas possíveis para o jogador
verifica_jogadas(_, _, _, []).
verifica_jogadas(Tabuleiro, Jogador, Consecutivas, [Jogada|Resto]) :-
    jogar_pecas(Jogador, Jogada, Tabuleiro, NovoTabuleiro),
    not(vencedor(NovoTabuleiro, Consecutivas, Jogador)),
    verifica_jogadas(Tabuleiro, Jogador, Consecutivas, Resto).
verifica_jogadas(Tabuleiro, Jogador, Consecutivas, [Jogada|_]) :-
    jogar_pecas(Jogador, Jogada, Tabuleiro, NovoTabuleiro),
    vencedor(NovoTabuleiro, Consecutivas, Jogador).




jogar(Largura, Altura, Consecutivas, Tabuleiro, Coluna, Linha, Peca, Versao, NovoTabuleiro) :-
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
    verificar_vitoria_horizontal(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    verificar_vitoria_vertical(Tabuleiro, Consecutivas, Peca).
vencedor(Tabuleiro, Consecutivas, Peca) :-
    verificar_vitoria_diagonal(Tabuleiro, Consecutivas, Peca).



:- jogar.