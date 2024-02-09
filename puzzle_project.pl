% Miguel Teixeira, ist1103449

:- [codigo_comum].

% ------------------------------------------------------------------------------
% extrai_ilhas_linha(N_L, Linha, Ilhas)
% Extrai uma lista ordenada de ilhas (Ilhas) a partir do numero da linha (N_L)  
% e de uma lista correspondente a uma linha de um puzzle (Linha)
% ------------------------------------------------------------------------------
extrai_ilhas_linha(_, [], _).

extrai_ilhas_linha(N_L, Linha, Ilhas):-
    length(Linha, N_C),
    extrai_ilhas_linha(N_L, Linha, Ilhas, [], N_C).

extrai_ilhas_linha(_, [], Ilhas, Ilhas, _). 

% Caso em que seja diferente de 0, ou seja uma ilha
extrai_ilhas_linha(N_L, [P|R], Ilhas, Aux, N_C):-
    P \== 0,
    length(R, N_C2),
    Col is (N_C - N_C2),
    append(Aux,[ilha(P,(N_L,Col))],IlhasNew),
    extrai_ilhas_linha(N_L, R, Ilhas, IlhasNew, N_C).

% Caso em que seja 0 
extrai_ilhas_linha(N_L, [P|R], Ilhas, Aux, N_C):-
    P == 0,
    extrai_ilhas_linha(N_L, R, Ilhas, Aux, N_C).

% ------------------------------------------------------------------------------
% ilhas(Puz, Ilhas)
% O objetivo deste predicado e criar uma lista com as ilhas (Ilhas) de um dado 
% Puzzle (Puz)
% Ilhas e a lista ordenada (da esquerda para a direita e de cima para baixo) 
% das ilhas de Puz
% ------------------------------------------------------------------------------
ilhas([], _).

ilhas(Puz, Ilhas):-
    N_L is 0,
    ilhas(Puz, Ilhas, [], N_L).

ilhas([], Ilhas, Ilhas, _).

ilhas([P|R], Ilhas, Aux, N_L):-
    N_L_New is (N_L + 1),
    extrai_ilhas_linha(N_L_New, P, Y),
    append(Aux, Y, Ilhas_New),
    ilhas(R, Ilhas, Ilhas_New, N_L_New).

% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% O objetivo deste predicado e criar uma lista ordenada (Vizinhas) com as 
% vizinhas da ilha (Ilha), tendo em conta as outras ilhas existentes (Ilhas)
% Duas ilhas dizem-se vizinhas se: se encontrarem na mesma linha ou mesma coluna
% e entre elas nao existir outra ilha
% ------------------------------------------------------------------------------

%vizinhas(Ilhas, Ilha, Vizinhas):-
%   Ilha = ilha(_,(Linha,Coluna)),
%    findall(Aux, (member(Aux, Ilhas), 
%                 Aux \= Ilha, ((   
%                               Aux = ilha(_,(_, Coluna)); Aux = 
%                               ilha(_,(Linha, _)))) ), Vizinhas).


% ------------------------------------------------------------------------------
% estado(Ilhas, Estado)
% O objetivo deste predicado e criar um estado (Estado) a partir das ilhas 
% existentes (Ilhas)
% Um estado e uma lista de entradas; uma entrada e uma lista em que: o primeiro 
% elemento e uma ilha, o segundo elemento e uma lista de vizinhas dessa ilha
% e o terceiro elemento e uma lista das pontes da ilha (inicialmente e vazia)
% ------------------------------------------------------------------------------
% estado([], _).

% estado(Ilhas, Estado):-
%    estado(Ilhas, Estado, []).

% estado([], Estado, Estado).

% estado([P|R], Estado, Aux):-
%    append(Aux, [P, vizinhas(Ilhas, P, Vizinhas), []], Estado_New),
%    estado(R, Estado, Estado_New).

% ------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2, excluindo Pos1 
% e Pos2
% Se nao existir nenhum elemento, falha e gera false
% ------------------------------------------------------------------------------
:- discontiguous posicoes_entre/3.

% Mesma linha
posicoes_entre(Pos1, Pos2, Aux):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (N =:= N_2),
    F_2 > F,
    findall(Aux, (between(F, F_2, Aux)), Mesmo_Lin),
    delete(Mesmo_Lin, F , Mesmo_Lin_New),
    delete(Mesmo_Lin_New, F_2 , Mesmo_Lin_New2),
    Mesmo_Lin_New2 = [P|R],
    Posicoes1 = [(N, P)],
    auxiliar_f(N, R, Posicoes1, Aux).

auxiliar_f(_, [], Aux, Aux):- !.
auxiliar_f(X, [S|T], Posicoes, Aux):-
    append(Posicoes, [(X, S)], PosicoesNew),
    auxiliar_f(X, T, PosicoesNew, Aux).

% Quando a Pos1 e maior que a Pos2
posicoes_entre(Pos1, Pos2, Aux):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (N =:= N_2),
    F_2 < F,
    findall(Aux, (between(F_2, F, Aux)), Mesmo_Lin),
    delete(Mesmo_Lin, F , Mesmo_Lin_New),
    delete(Mesmo_Lin_New, F_2 , Mesmo_Lin_New2),
    Mesmo_Lin_New2 = [P|R],
    Posicoes1 = [(N, P)],
    auxiliar_f(N_2, R, Posicoes1, Aux).

% Mesma coluna
posicoes_entre(Pos1, Pos2, Aux):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (F =:= F_2),
    N_2 > N,
    findall(Aux, (between(N, N_2, Aux)), Mesmo_Col),
    delete(Mesmo_Col, N , Mesmo_Col_New),
    delete(Mesmo_Col_New, N_2 , Mesmo_Col_New2),
    Mesmo_Col_New2 = [P|R],
    Posicoes1 = [(P, F)],
    auxiliar_f2(F, R, Posicoes1, Aux).

auxiliar_f2(_, [], Aux, Aux):- !.
auxiliar_f2(X, [S|T], Posicoes, Aux):-
    append(Posicoes, [(S, X)], PosicoesNew),
    auxiliar_f2(X, T, PosicoesNew, Aux).

% Quando a Pos1 e maior que a Pos2
posicoes_entre(Pos1, Pos2, Aux):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (F =:= F_2),
    N_2 < N,
    findall(Aux, (between(N_2, N, Aux)), Mesmo_Col),
    delete(Mesmo_Col, N , Mesmo_Col_New),
    delete(Mesmo_Col_New, N_2 , Mesmo_Col_New2),
    Mesmo_Col_New2 = [P|R],
    Posicoes1 = [(P, F)],
    auxiliar_f2(F_2, R, Posicoes1, Aux).

% ------------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% Pos1 e Pos2 sao 2 posicoes
% Ponte e uma ponte entre essas 2 posicoes (ordenadas)
% ------------------------------------------------------------------------------
cria_ponte(Pos1, Pos2, Ponte):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (N =:= N_2),
    F > F_2,
    Ponte = ponte(Pos2, Pos1).

cria_ponte(Pos1, Pos2, Ponte):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (N =:= N_2),
    F < F_2,
    Ponte = ponte(Pos1, Pos2).

cria_ponte(Pos1, Pos2, Ponte):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (F =:= F_2),
    N > N_2,
    Ponte = ponte(Pos2, Pos1).

cria_ponte(Pos1, Pos2, Ponte):-
    Pos1 = (N, F),
    Pos2 = (N_2, F_2),
    (F =:= F_2),
    N < N_2,
    Ponte = ponte(Pos1, Pos2).

% ------------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% Pos1 e Pos2 sao posicoes, Posicoes e a lista ordenada de posicoes entre Pos1 e 
% Pos2, I e uma ilha e Vz e uma das suas vizinhas, 
% significa que a adicao da ponte ponte(Pos1, Pos2) nao faz com que I e 
% Vz deixem de ser vizinhas.
% ------------------------------------------------------------------------------
caminho_livre(_, _, Posicoes, I, Vz):-
    I = ilha(_,(Li,Ci)),
    Vz = ilha(_,(Lvz,Cvz)),
    posicoes_entre((Li,Ci), (Lvz,Cvz), Aux),
    ((Posicoes = Aux); (caminho_livre_aux(Aux, Posicoes))).

caminho_livre_aux([],_).
caminho_livre_aux(List1, List2):-
    List1 = [P|R],
    \+ (member(P, List2)),
    caminho_livre_aux(R, List2).

% ------------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% Posicoes sao as posicoes entre Pos1 e Pos2 e Entrada e uma entrada
% A Nova_Entrada e igual a Entrada excepto que lhe foram removidas as ilhas que 
% deixaram de ser vizinhas se fosse adicionada uma ponte entre Pos1 e Pos2
% ------------------------------------------------------------------------------
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada):-
    Entrada = [Ilha, LstVizinhas, LstPontes], % Pontes inicialmente []
    actualiza_vizinhas_entrada_aux(Pos1, Pos2, Posicoes, Ilha, LstVizinhas, 
        LstVizinhasNew),
    Nova_Entrada = [Ilha, LstVizinhasNew, LstPontes].

actualiza_vizinhas_entrada_aux(_, _, _, _, [], _).

% se o caminho estiver livre
actualiza_vizinhas_entrada_aux(Pos1, Pos2, Posicoes, Ilha, LstVizinhas, Aux):-
    LstVizinhas = [P|R],
    caminho_livre(Pos1, Pos2, Posicoes, Ilha, P),
    append([P], [], Aux),
    actualiza_vizinhas_entrada_aux(Pos1, Pos2, Posicoes, Ilha, R, Aux).

% se o caminho nao estiver livre
actualiza_vizinhas_entrada_aux(Pos1, Pos2, Posicoes, Ilha, LstVizinhas, Aux):-
    LstVizinhas = [P|R],
    \+ (caminho_livre(Pos1, Pos2, Posicoes, Ilha, P)),
    Aux = [],
    actualiza_vizinhas_entrada_aux(Pos1, Pos2, Posicoes, Ilha, R, Aux).

% ------------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% Estado e um estado, Pos1 e Pos2 sao as posicoes entre as quais foi adicionada 
% uma ponte 
% Novo_estado e o estado que se obtem de Estado apos a actualizacao das ilhas
% vizinhas de cada uma das suas entradas
% ------------------------------------------------------------------------------

% actualiza_vizinhas_apos_pontes([], _, _, _).

% actualiza_vizinhas_apos_pontes([P|R], Pos1, Pos2, Novo_estado):-
 %   posicoes_entre(Pos1, Pos2, Posicoes),
 %   delete((ilha(_,Pos1),ilha(_,Pos2)), P),
 %  actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, P, NovoP),
 %  append(NovoP, [], Novo_estado),
 %  actualiza_vizinhas_apos_pontes(R, Pos1, Pos2, Novo_estado).

% ------------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% Estado e um estado, Ilhas_term e a lista de ilhas de Estado que ja tem todas 
% as pontes, isto e, e a lista das ilhas terminadas
% Uma ilha esta terminada se Num_pontes for diferente de X e
% o comprimento da lista Pontes for Num_pontes
% ------------------------------------------------------------------------------
ilhas_terminadas(Estado, Ilhas_term):-
    ilhas_terminadas_aux(Estado, Ilhas_term, []).

ilhas_terminadas_aux([], Ilhas_term, Ilhas_term).
    
% caso em que a ilha esta terminada
ilhas_terminadas_aux([P|R], Ilhas_term, Aux):-
    P = [ilha(Y,Z),_, Pontes],
    Y \= 'X',
    length(Pontes,Aux2),
    Aux2 =:= Y,
    append(Aux, [ilha(Y,Z)], AuxNew),
    ilhas_terminadas_aux(R, Ilhas_term, AuxNew).

% caso em que Num_pontes seja X ou nao seja o comprimento de Pontes
ilhas_terminadas_aux([_|R], Ilhas_term, Aux):-
    ilhas_terminadas_aux(R, Ilhas_term, Aux).

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Ilhas_term e uma lista de ilhas terminadas
% Entrada e uma entrada
% Nova_entrada e a entrada resultante de remover as ilhas de
% Ilhas_term, da lista de ilhas vizinhas de entrada
% ------------------------------------------------------------------------------
tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada):-
    Entrada = [Ilha, Vizinhas, _],
    subtract(Vizinhas, Ilhas_term, Aux),
    Nova_entrada =  [Ilha, Aux, []].

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Ilhas_term e uma lista de ilhas terminadas
% Estado e um estado
% Novo_Estado e o estado resultante de aplicar tira_ilhas_terminadas_entrada a 
% cada uma das entradas de Estado
% ------------------------------------------------------------------------------
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    tira_ilhas_terminadas_aux(Estado, Ilhas_term, Novo_estado, []).

tira_ilhas_terminadas_aux([], _, Novo_estado, Novo_estado).

tira_ilhas_terminadas_aux([P|R], Ilhas_term, Novo_estado, Aux):-
    tira_ilhas_terminadas_entrada(Ilhas_term, P, NewP),
    append(Aux, [NewP], AuxNew),
    tira_ilhas_terminadas_aux(R, Ilhas_term, Novo_estado, AuxNew).

% ------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Ilhas_term e uma lista de ilhas terminadas
% Entrada e uma entrada
% Nova_entrada e a entrada resultante do seguinte: se a ilha de Entrada 
% pertencer a Ilhas_term, o seu numero de pontes deve ser substituido por X
% ------------------------------------------------------------------------------

% a ilha de Entrada pertence a Ilhas_term
marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada):-
    Entrada = [Ilha, Vizinhas, Pontes],
    Ilha = ilha(_, Pos),
    member(Ilha, Ilhas_term),
    Nova_entrada = [ilha('X', Pos), Vizinhas, Pontes].

% a ilha de Entrada nao pertence a Ilhas_term
marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada):-
    Entrada = [Ilha, _, _],
    \+ (member(Ilha, Ilhas_term)),
    Nova_entrada = Entrada.

% ------------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Ilhas_term e uma lista de ilhas terminadas
% Estado e um estado
% Novo_estado e o estado resultante de aplicar marca_ilhas_terminadas_entrada a 
% cada uma das entradas de Estado 
% ------------------------------------------------------------------------------
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado):-
    marca_ilhas_terminadas_aux(Estado, Ilhas_term, Novo_estado, []).

marca_ilhas_terminadas_aux([], _, Novo_estado, Novo_estado).

marca_ilhas_terminadas_aux([P|R], Ilhas_term, Novo_estado, Aux):-
    marca_ilhas_terminadas_entrada(Ilhas_term, P, NewP),
    append(Aux, [NewP], AuxNew),
    marca_ilhas_terminadas_aux(R, Ilhas_term, Novo_estado, AuxNew).

% ------------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado)
% Estado e um estado
% Novo_estado e o estado resultante de aplicar os predicados 
% tira_ilhas_terminadas e marca_ilhas_terminadas a Estado 
% ------------------------------------------------------------------------------

% primeiro tenho de ver quais sao as ilhas terminadas
% trata_ilhas_terminadas(Estado, Novo_estado):-
%    ilhas_terminadas(Estado, Ilhas_term),
%    trata_ilhas_terminadas_aux(Estado, Ilhas_term, Novo_estado, []).

% trata_ilhas_terminadas_aux([], _, Novo_estado, Novo_estado).

% trata_ilhas_terminadas_aux([P|R], Ilhas_term, Novo_estado, Aux):-
%    tira_ilhas_terminadas([P], Ilhas_term, NewC),
%    marca_ilhas_terminadas(NewC, Ilhas_term, NewP),
%    append(Aux, NewP, AuxNew),
%    trata_ilhas_terminadas_aux(R, Ilhas_term, Novo_estado, AuxNew).

% ------------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
% Este predicado executa os seguintes passos:
% Cria a(s) ponte(s) entre Ilha1 e Ilha2
% Adiciona as novas pontes as entradas de Estado correspondentes a estas ilhas
% Atualiza o estado por aplicacao dos predicados actualiza_vizinhas_apos_pontes 
% e trata_ilhas_terminadas
% ------------------------------------------------------------------------------

% Fim
% Miguel Teixeira, ist1103449 

% ------------------------------------------------------------------------------