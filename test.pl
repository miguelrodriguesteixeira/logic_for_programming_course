% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% O objetivo deste predicado e criar uma lista ordenada (Vizinhas) com as 
% vizinhas da ilha (Ilha), tendo em conta as outras ilhas existentes (Ilhas)
% Duas ilhas dizem-se vizinhas se: se encontrarem na mesma linha ou mesma coluna;
% entre elas não existir outra ilha
% ------------------------------------------------------------------------------

% findall(Template,Goal,List)

vizinhas(Ilhas, Ilha, Vizinhas):-
    Ilha = ilha(_,(Linha,Coluna)),
    findall(Aux, (member(Aux, Ilhas), 
                 Aux \= Ilha, ((   
                               Aux = ilha(_,(_, Coluna)); Aux = ilha(_,(Linha, _)))) ), Vizinhas).

%last  ulitmo elemento dalista

---------------------------------------
vizinhas(Ilhas, Ilha, Vizinhas):-
    Ilha = ilha(_,(_,Coluna)),
    findall(Aux, (member(Aux, Ilhas), 
                 Aux \= Ilha, (Aux = ilha(_,(_, Coluna)))), Mesmo_Col),
    Mesmo_Col is [P|R],
    P is ilha(_,(_,Coluna_P)),
    findall(Ax, (member(Ax, Mesmo_Col),(Coluna_P < Coluna)), Menores_Col),
    Menores_Col is [F|S],
    
    

vizinhas(Ilhas, Ilha, Vizinhas):-
    Ilha = ilha(_,(Linha,_)),
    findall(Aux, (member(Aux, Ilhas), 
                 Aux \= Ilha, (Aux = ilha(_,(Linha, _)))), Mesmo_Lin),
    Mesmo_Lin is [P|R],

-----------

vizinhas(Ilhas, Ilha, Vizinhas):-
    vizinhas(Ilhas, Ilha, _, Vizinhas).

vizinhas(Ilhas, Ilha, _, Vizinhas):-
    vizinhasBaixo(Ilhas, Ilha, ElementoBaixo),
    vizinhasCima(Ilhas, Ilha, ElementoCima),
    vizinhasDir(Ilhas, Ilha, ElementoDir),
    vizinhasEsq(Ilhas, Ilha, ElementoEsq),
    append([ElementoBaixo, ElementoCimam ElementoDir, ElementoEsq], Vizinhas).

-------------------------------------
vizinhas(Ilhas, Ilha, Vizinhas):-
    Ilha = ilha(_,(Linha,Coluna)),
    findall(Aux, (member(Aux, Ilhas), 
                 Aux \= Ilha, ((   
                               Aux = ilha(_,(_, Coluna)); Aux = ilha(_,(Linha, _)))) ), Vizinhas),
    vizinhas_aux_direita(Ilha, Vizinhas, [], []).

vizinhas_aux_direita(_, Aux2, _, Aux2).
vizinhas_aux_direita(Island, Lista, VizinhasNew, Aux2):-
    Lista = [P|R],
    Island = ilha(_,(Linha,Coluna)),
    P = ilha(_,(Lin2,Col2)),
    (Linha =:= Lin2),
    Col2 > Coluna, 
    append([P], VizinhasNew, Aux),
    vizinhas_aux_direita(Island, R, Aux, Aux2),
    Aux = [Q|_],
    Aux = [Q].


