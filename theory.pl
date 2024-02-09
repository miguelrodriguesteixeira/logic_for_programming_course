/* 
Encontra os impares e devolve a sua posicao mais o impar que encontrou vezes 200 
na forma ola(Pos_Impar, Resultado_Da_Muliplicacao_Por_200).
*/

encontra_pos_impares(Lista, Resultado) :- 
    length(Lista, N),
    findall(ola(Pos, ImparXDois), (between(1, N, Pos), 
            nth1(Pos, Lista, Candidato),
            is_odd(Candidato), ImparXDois is Candidato * 200), 
            Resultado).

is_odd(I) :- 0 =\= I mod 2.

/* de novo o encontra_pos_impares, mas em versao mais tradicional
 Vai ter dois acumuladores (versao iterativa) */
 
encontra_pos_impares_versao_2(Lista, Resultado) :- 
    encontra_pos_impares_aux(Lista, Resultado, 1, []). % 1 para a pos inicial    

% Base de recursao
encontra_pos_impares_aux([], Resultado, _, Resultado).

% Caso em que a head e um impar
encontra_pos_impares_aux([Head | Tail], Resultado, PosAux, ResultadoTmp) :-
    is_odd(Head), !,
    ImparXDois is Head * 200,
    append(ResultadoTmp, [ola(PosAux, ImparXDois)], ResultadoTmpNew),
    PosAux1 is PosAux + 1,
    encontra_pos_impares_aux(Tail, Resultado, PosAux1, ResultadoTmpNew).

% Caso em que a Head e um par
encontra_pos_impares_aux([_ | Tail], Resultado, PosAux, ResultadoTmp) :-
    PosAux1 is PosAux + 1,
    encontra_pos_impares_aux(Tail, Resultado, PosAux1, ResultadoTmp).

/* Versão anterior, mas um a um. 
Podia usar-se o member se a pos nao fosse importante */

encontra_pos_impares_um_a_um(Lista, Resultado) :- 
    length(Lista, N), 
    between(1, N, Pos),
    nth1(Pos, Lista, Candidato),
    is_odd(Candidato), 
    ImparXDois is Candidato * 200,
    Resultado = ola(Pos, ImparXDois).

/*
?- encontra_pos_impares_versao_2([1, 2, 5, 10, 4, 2, 3], Resultado).
Resultado = [ola(1, 200), ola(3, 1000), ola(7, 600)].

?- encontra_pos_impares([1, 2, 5, 10, 4, 2, 3], Resultado).
Resultado = [ola(1, 200), ola(3, 1000), ola(7, 600)].

?- encontra_pos_impares_um_a_um([1, 2, 5, 10, 4, 2, 3], Resultado).
Resultado = ola(1, 200) ;
Resultado = ola(3, 1000) ;
Resultado = ola(7, 600).
*/



