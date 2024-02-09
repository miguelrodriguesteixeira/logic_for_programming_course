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
trata_ilhas_terminadas(Estado, Novo_estado):-
    ilhas_terminadas(Estado, Ilhas_term),
    trata_ilhas_terminadas_aux(Estado, Ilhas_term, Novo_estado, []).

trata_ilhas_terminadas_aux([], _, Novo_estado, Novo_estado).

trata_ilhas_terminadas_aux([P|R], Ilhas_term, Novo_estado, Aux):-
    marca_ilhas_terminadas([P], Ilhas_term, NewC),
    tira_ilhas_terminadas(NewC, Ilhas_term, NewP),
    append(Aux, NewP, AuxNew),
    trata_ilhas_terminadas_aux(R, Ilhas_term, Novo_estado, AuxNew).
