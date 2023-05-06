% Aloízio Pita de Castro Júnior - 201365006C
% Vinícius de Oliveira Corbelli - 202065093C

:- use_module(library(clpfd)).
:- use_module(library(pce)).

mostra_aviso(Mensagem) :-
    new(Janela, dialog('Alerta')),
    send(Janela, append, new(Texto, text(Mensagem))),
    send(Texto, font, font(helvetica, bold, 14)),
    send(Janela, append, button('Ok', message(Janela, destroy))),
    send(Janela, default_button, 'Ok'),
    send(Janela, open).

% Definir o predicado que fecha o menu
fechar_menu(Menu) :-
    send(Menu, destroy).

% Predicado principal do jogo
jogar :-
    new(Menu, dialog('Jogo da Velha', size(200, 200))),
    new(QuantidadeLinhas, int_item('Quantidade de Linhas')),
    send(Menu, append, QuantidadeLinhas),

    new(Normal, button(normal, and(message(@prolog, jogo_normal, QuantidadeLinhas?selection),
                                    message(@prolog, fechar_menu, Menu)))),
    send(Menu, append, Normal),

    new(Simplificada, button(simplificada, and(message(@prolog, jogo_simplificada, QuantidadeLinhas?selection),
                                    message(@prolog, fechar_menu, Menu)))),
    send(Menu, append, Simplificada),

    send(Menu, open).

jogo_normal(Altura) :-
    Largura is Altura + 1,
    Consecutivas is Altura,
    cria_tabuleiro(Altura, Largura, Tabuleiro),
    imprime_tabuleiro_v1(Altura, Largura, Consecutivas, Tabuleiro).

jogo_simplificada(Altura) :-
    Largura is Altura + 1,
    Consecutivas is Altura,
    cria_tabuleiro(Altura, Largura, Tabuleiro),
    imprime_tabuleiro_v2(Altura, Largura, Consecutivas, Tabuleiro).

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

imprime_tabuleiro_v1(Altura, Largura, Consecutivas, Tabuleiro) :-
    new(Window, dialog('Jogo da Velha', size(Altura * 50, Largura * 50))),
    cria_botao_tabuleiro_v1(1, 1, Altura, Largura, Consecutivas, Tabuleiro, Window),
    send(Window, open).

% Predicado auxiliar para criar o botão de uma posição
cria_botao_posicao_v1(X, Y, Janela, Tabuleiro, Altura, Largura, Consecutivas) :-
    get_posicao(X, Y, Tabuleiro, Simbolo),

    new(Botao, button(Simbolo, and(message(@prolog, joga_v1, Altura, Largura, Consecutivas, Tabuleiro, X, Y),
                                    message(@prolog, fechar_menu, Janela)))),
    send(Janela, append, Botao).

% Predicado auxiliar para criar os botões de uma linha
cria_botao_linha_v1(X, Y, Altura, Largura, Consecutivas, Janela, Tabuleiro) :-
    cria_botao_posicao_v1(X, Y, Janela, Tabuleiro, Altura, Largura, Consecutivas),
    Y1 is Y + 1,
    (Y1 =< Largura ->
    cria_botao_linha_v1(X, Y1, Altura, Largura, Consecutivas, Janela, Tabuleiro)
    ; send(Janela, append, new(Linea, text('\n')))).

% Predicado para criar os botões de todas as linhas
cria_botao_tabuleiro_v1(X, _, Altura, _, _, _, _) :- X > Altura, !.
cria_botao_tabuleiro_v1(X, Y, Altura, Largura, Consecutivas, Tabuleiro, Janela) :-
    cria_botao_linha_v1(X, Y, Altura, Largura, Consecutivas, Janela, Tabuleiro),
    X1 is X + 1,
    cria_botao_tabuleiro_v1(X1, Y, Altura, Largura, Consecutivas, Tabuleiro, Janela).


imprime_tabuleiro_v2(Altura, Largura, Consecutivas, Tabuleiro) :-
    new(Window, dialog('Jogo da Velha', size(Altura * 50, Largura * 50))),
    cria_botao_tabuleiro_v2(1, 1, Altura, Largura, Consecutivas, Tabuleiro, Window),
    send(Window, open).

% Predicado auxiliar para criar o botão de uma posição
cria_botao_posicao_v2(X, Y, Janela, Tabuleiro, Altura, Largura, Consecutivas) :-
    get_posicao(X, Y, Tabuleiro, Simbolo),

    new(Botao, button(Simbolo, and(message(@prolog, joga_v2, Altura, Largura, Consecutivas, Tabuleiro, Y),
                                    message(@prolog, fechar_menu, Janela)))),
    send(Janela, append, Botao).

% Predicado auxiliar para criar os botões de uma linha
cria_botao_linha_v2(X, Y, Altura, Largura, Consecutivas, Janela, Tabuleiro) :-
    cria_botao_posicao_v2(X, Y, Janela, Tabuleiro, Altura, Largura, Consecutivas),
    Y1 is Y + 1,
    (Y1 =< Largura ->
    cria_botao_linha_v2(X, Y1, Altura, Largura, Consecutivas, Janela, Tabuleiro)
    ; send(Janela, append, new(Linea, text('\n')))).

% Predicado para criar os botões de todas as linhas
cria_botao_tabuleiro_v2(X, _, Altura, _, _, _, _) :- X > Altura, !.
cria_botao_tabuleiro_v2(X, Y, Altura, Largura, Consecutivas, Tabuleiro, Janela) :-
    cria_botao_linha_v2(X, Y, Altura, Largura, Consecutivas, Janela, Tabuleiro),
    X1 is X + 1,
    cria_botao_tabuleiro_v2(X1, Y, Altura, Largura, Consecutivas, Tabuleiro, Janela).


% Predicado para buscar o símbolo de uma posição do tabuleiro
get_posicao(X, Y, Tabuleiro, Simbolo) :-
    nth1(X, Tabuleiro, Linha),
    nth1(Y, Linha, Simbolo),
    (var(Simbolo) -> Simbolo = 'N' ; true).

% Predicado para substituir um elemento em uma posição específica
troca([Linha|Linhas], 1, Coluna, [NovaLinha|Linhas], Simbolo) :-
    troca_linha(Linha, Coluna, Simbolo, NovaLinha).
troca([Linha|Linhas], NumLinha, Coluna, [Linha|NovasLinhas], Simbolo) :-
    NumLinha > 1,
    NumLinha1 is NumLinha - 1,
    troca(Linhas, NumLinha1, Coluna, NovasLinhas, Simbolo).

% Predicado para substituir um elemento em uma linha espec�fica
troca_linha([_|Colunas], 1, Simbolo, [Simbolo|Colunas]).
troca_linha([Coluna|Colunas], NumColuna, Simbolo, [Coluna|NovasColunas]) :-
    NumColuna =\= 1,
    NumColuna1 is NumColuna - 1,
    troca_linha(Colunas, NumColuna1, Simbolo, NovasColunas).

% PREDICADOS PARA A VERSAO 1 DO JOGO DA VELHA

% Predicado que tenta buscar simbolo na posicao m x n do tabuleiro
verifica_simbolo(Tabuleiro, Linha, Coluna):-
    nth1(Linha, Tabuleiro, LinhaTabuleiro),
    nth1(Coluna, LinhaTabuleiro, PosSim),
    not(simbolo(PosSim)).

% Predicado para fazer jogada
joga_v1(Altura, Largura, Consecutivas, Tabuleiro, Linha, Coluna) :-
    mostra_aviso('0'),
    (verifica_simbolo(Tabuleiro, Linha, Coluna)) ->
    mostra_aviso('1'),
    troca(Tabuleiro, Linha, Coluna, NovoTabuleiro, 'X'),
    imprime_tabuleiro_v1(Altura, Largura, Consecutivas, NovoTabuleiro).
    not(vitoria_horizontal(NovoTabuleiro, Linha, 'X', 1)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'X', 1)),
    not(vitoria_diagonal(NovoTabuleiro, 'X', 1, Largura)),
    joga2_v1(Altura, Largura, Consecutivas, NovoTabuleiro);
    mostra_aviso('Posicao ja preenchida, escolha outra'),
    imprime_tabuleiro_v1(Altura, Largura, Consecutivas, Tabuleiro).

% Jogador 2 joga
joga2_v1(Altura, Largura, Consecutivas, Tabuleiro) :-
    % Encontra a primeira posição vazia em que o jogador 2 pode jogar e vencer
    jogada_vencedora(Tabuleiro, Altura, Largura, Consecutivas, Linha, Coluna),
    troca(Tabuleiro, Linha, Coluna, NovoTabuleiro, 'O'),
    not(vitoria_horizontal(NovoTabuleiro, Linha, 'O', 2)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'O', 2)),
    not(vitoria_diagonal(NovoTabuleiro, 'O', 2, Largura)),
    imprime_tabuleiro_v1(Altura, Largura, Consecutivas, NovoTabuleiro).

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
    member('N', ColunaEscolhida).

% Predicado para fazer jogada
joga_v2(Altura, Largura, Consecutivas, Tabuleiro, Coluna) :-
    (verifica_simbolo(Tabuleiro, Linha, Coluna)) ->
    troca_v2(Tabuleiro, Coluna, 'X', NovoTabuleiro, Indice),
    imprime_tabuleiro(NovoTabuleiro),
    not(vitoria_horizontal(NovoTabuleiro, Linha, 'X', 1)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'X', 1)),
    not(vitoria_diagonal(NovoTabuleiro, 'X', 1, Largura)),
    joga2_v2(Altura, Largura, Consecutivas, NovoTabuleiro);
    mostra_aviso('Coluna selecionada esta cheia, selecione outra'),
    imprime_tabuleiro_v2(Altura, Largura, Consecutivas, Tabuleiro).

% Jogador 2 joga
joga2_v2(Altura, Largura, Consecutivas, Tabuleiro) :-
    % Encontra a primeira posição vazia em que o jogador 2 pode jogar e vencer
    jogada_vencedora_v2(Tabuleiro, Altura, Largura, Consecutivas, Linha, Coluna),
    troca(Tabuleiro, Linha, Coluna, NovoTabuleiro, 'O'),
    not(vitoria_horizontal(NovoTabuleiro, Linha, 'O', 2)),
    not(vitoria_vertical(NovoTabuleiro, Coluna, 'O', 2)),
    not(vitoria_diagonal(NovoTabuleiro, 'O', 2, Largura)),
    imprime_tabuleiro_v2(Altura, Largura, Consecutivas, NovoTabuleiro).

% PREDICADOS PARA DETERMINAR VENCEDOR

% Verifica vencedor na horizontal
vitoria_horizontal(Tabuleiro, Linha, Simbolo, Jogador) :-
    Linha1 is Linha - 1,
    nth0(Linha1, Tabuleiro, LinhaTeste),
    mesmo_simbolo(LinhaTeste, Simbolo),
    mostra_aviso(concat_atom(['Vitoria do jogador:', Jogador])),
    jogar.

% Verifica vencedor na vertical
vitoria_vertical(Tabuleiro, Coluna, Simbolo, Jogador) :-
    Coluna1 is Coluna - 1,
    transpose(Tabuleiro, Transposta),
    nth0(Coluna1, Transposta, LinhaTeste),
    mesmo_simbolo(LinhaTeste, Simbolo),
    mostra_aviso(concat_atom(['Vitoria do jogador:', Jogador])),
    jogar.

% Verifica vencedor nas diagonais
vitoria_diagonal(Tabuleiro, Simbolo, Jogador, Largura):-
    pega_diagonal1(Tabuleiro, 1, Diagonal1),
    pega_diagonal1(Tabuleiro, 2, Diagonal2),
    pega_diagonal2(Tabuleiro, Largura, Diagonal3),
    Largura1 is Largura - 1,
    pega_diagonal2(Tabuleiro, Largura1, Diagonal4),
    (mesmo_simbolo(Diagonal1, Simbolo); mesmo_simbolo(Diagonal2, Simbolo); mesmo_simbolo(Diagonal3, Simbolo) ; mesmo_simbolo(Diagonal4, Simbolo)) ->
    mostra_aviso(concat_atom(['Vitoria do jogador:', Jogador])),
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


% Verifica empate se nao houver qualquer 'N' no tabuleiro
empate(Tabuleiro) :-
    converte_tabuleiro(Tabuleiro, Lista),
    not(member('N', Lista)) ->
    mostra_aviso('A partida empatou'),
    jogar;

% Converte o tabuleiro m x m mais 1 por uma lista simples
converte_tabuleiro(Tabuleiro, Lista_Tabuleiro):-
    converte_tabuleiro(Tabuleiro, [], Lista_Tabuleiro).

converte_tabuleiro([], Acumulador, Acumulador).

converte_tabuleiro([Tabuleiro|Resto], AcumuladorParcial, Lista_Tabuleiro) :-
    append(AcumuladorParcial, Tabuleiro, NovoAcumulador),
    converte_tabuleiro(Resto, NovoAcumulador, Lista_Tabuleiro).

% Verifica se simbolos da lista sao iguais
mesmo_simbolo([],_).
mesmo_simbolo([H|T], H) :- mesmo_simbolo(T,H).

jogada_vencedora(Tabuleiro, Altura, Largura, Consecutivas, Linha, Coluna) :-
    findall([L, C],
            (between(1, Altura, L),
             between(1, Largura, C),
             verifica_simbolo(Tabuleiro, L, C),
             troca(Tabuleiro, L, C, NovoTabuleiro, 'O'),
             (vitoria_horizontal_aux(NovoTabuleiro, L, 'O', 2);
              vitoria_vertical_aux(NovoTabuleiro, C, 'O', 2);
              vitoria_diagonal(NovoTabuleiro, 'O', 2, Largura))),
            JogadasVitoria),
    (   member([Linha, Coluna], JogadasVitoria),
        ! % se houver uma jogada que leva à vitória imediata, retorne essa jogada
        ;
        % caso contrário, escolha a jogada que maximize as chances de vencer no próximo turno

        findall([L, C],
            (between(1, Altura, L),
             between(1, Largura, C),
             verifica_simbolo(Tabuleiro, L, C)),
            JogadasValidas),

        findall([L, C, P],
                (member([L, C], JogadasValidas),
                 troca(Tabuleiro, L, C, NovoTabuleiro, 'O'),
                 pontuacao_jogada(NovoTabuleiro, Altura, Largura, Consecutivas, L, C, JogadasValidas, P)),
                JogadasPontuadas),
                
        sort(JogadasPontuadas, SortedJogadasPontuadas),
        reverse(SortedJogadasPontuadas, ReverseJogadasPontuadas),
        member([Linha, Coluna, _], ReverseJogadasPontuadas)
    ).

jogada_vencedora_v2(Tabuleiro, Altura, Largura, Consecutivas, Linha, Coluna) :-
    findall([C],
            (between(1, Largura, C),
             verifica_coluna(Tabuleiro, C),
             troca_v2(Tabuleiro, C, 'O', NovoTabuleiro, L),
             (vitoria_horizontal_aux(NovoTabuleiro, L, 'O', 2);
              vitoria_vertical_aux(NovoTabuleiro, C, 'O', 2);
              vitoria_diagonal(NovoTabuleiro, 'O', 2, Largura))),
            JogadasVitoria),
    (   member([Coluna], JogadasVitoria),
        ! % se houver uma jogada que leva à vitória imediata, retorne essa jogada
        ;
        % caso contrário, escolha a jogada que maximize as chances de vencer no próximo turno

        findall(C, (between(1, Largura, C), coluna_tem_espaco(Tabuleiro, C)), JogadasValidas),

         findall([C],
                (member(C, JogadasValidas),
                 troca_v2(Tabuleiro, C, 'O', NovoTabuleiro, L),
                 pontuacao_jogada(NovoTabuleiro, Altura, Largura, Consecutivas, L, C, JogadasValidas, P)),
                JogadasPontuadas),
                
        sort(JogadasPontuadas, SortedJogadasPontuadas),
        reverse(SortedJogadasPontuadas, ReverseJogadasPontuadas),
        member([Coluna], ReverseJogadasPontuadas)
    ).

pontuacao_jogada(Tabuleiro, Altura, Largura, Consecutivas, Linha, Coluna, JogadasValidas, Pontuacao) :-
    troca(Tabuleiro, Linha, Coluna, NovoTabuleiro, 'O'),
    findall(P,
            (member([L, C], JogadasValidas),
             troca(NovoTabuleiro, L, C, NovoTabuleiro2, 'O'),
             (vitoria_horizontal_aux(NovoTabuleiro2, L, 'O', Consecutivas);
              vitoria_vertical_aux(NovoTabuleiro2, C, 'O', Consecutivas);
              vitoria_diagonal_aux(NovoTabuleiro2, 'O', C, L, Consecutivas, Altura, Largura)),
             P),
            Pontos),
    length(Pontos, NumPontos),
    Pontuacao is NumPontos / (Altura * Largura).

coluna_tem_espaco(Tabuleiro, Coluna) :-
    nth1(1, Tabuleiro, PrimeiraLinha),
    nth1(Coluna, PrimeiraLinha, UltimoElemento),
    UltimoElemento == 'N'.

vitoria_horizontal_aux(Tabuleiro, Linha, Simbolo, Consecutivas) :-
    nth1(Linha, Tabuleiro, LinhaTabuleiro),
    append(_, Prefixo, LinhaTabuleiro),
    append(Simbolos, _, Prefixo),
    length(Simbolos, Contagem),
    Contagem >= Consecutivas,
    maplist(=(Simbolo), Simbolos).


vitoria_vertical_aux(Tabuleiro, Coluna, Simbolo, Consecutivas) :-
    transpose(Tabuleiro, TabuleiroTransposto),
    vitoria_horizontal_aux(TabuleiroTransposto, Coluna, Simbolo, Consecutivas).

vitoria_diagonal_aux(Tabuleiro, Simbolo, C, L, Consecutivas, Altura, Largura) :-
    % verifica diagonal principal
    nth1(1, Tabuleiro, Linha1),
    nth1(C, Linha1, PosSim),
    PosSim == Simbolo,
    LinhaIni is L - C + 1,
    LinhaFim is Altura - C + L,
    ColunaIni is 1,
    ColunaFim is Largura - Altura + L + C - 1,
    verifica_diagonal(Tabuleiro, Simbolo, LinhaIni, ColunaIni, LinhaFim, ColunaFim, 1, Consecutivas);
    % verifica diagonal secundária
    LarguraAux is Largura - 1,
    nth1(1, Tabuleiro, Linha2),
    nth1(LarguraAux, Linha2, PosSim2),
    PosSim2 == Simbolo,
    LinhaIni2 is L - C + 1,
    LinhaFim2 is Altura - C + L,
    ColunaIni2 is Largura - Altura + L + C - 1,
    ColunaFim2 is Largura - C,
    verifica_diagonal(Tabuleiro, Simbolo, LinhaIni2, ColunaIni2, LinhaFim2, ColunaFim2, 1, Consecutivas).
    
verifica_diagonal(_, _, LinhaIni, _, LinhaFim, _, Consecutivas, Consecutivas) :-
    LinhaIni > LinhaFim.
verifica_diagonal(Tabuleiro, Simbolo, LinhaIni, ColunaIni, LinhaFim, ColunaFim, Cont, Consecutivas) :-
    nth1(LinhaIni, Tabuleiro, Linha),
    nth1(ColunaIni, Linha, PosSim),
    PosSim == Simbolo,
    Cont1 is Cont + 1,
    (Cont1 >= Consecutivas -> true ; (LinhaIni1 is LinhaIni + 1, ColunaIni1 is ColunaIni + 1, verifica_diagonal(Tabuleiro, Simbolo, LinhaIni1, ColunaIni1, LinhaFim, ColunaFim, Cont1, Consecutivas))).

%:- jogar.
