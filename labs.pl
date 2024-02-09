% insere_ordenada(N, Lst, N_Lst):-
%  findall(Menores_aux,(member(Menores_aux,Lst), Menores_aux < N), Menores),
%  findall(Maiores_aux,(member(Maiores_aux,Lst), Maiores_aux >= N), Maiores),
%    append([Menores, [N], Maiores], N_Lst).

% findall(Template,Goal,List)


% junta_aleatorio(Lst1, Lim_inf, Lim_sup, Lst2):-
%%    findall(N, between(Lim_inf, Lim_sup, N),Todos),
 %   subtract(Todos, Lst1, Possiveis),
 %   length(Possiveis, Num_Possiveis),
  %  random_between(1, Num_Possiveis, Pos),
  %  nth1(Pos, Possiveis, Novo),
  %  insere_ordenada(Novo, Lst1, Lst2).

% maplist(:Goal, ?List1)
% aplica goal em lista1

%unifica(X,Y) :- X = Y.
%repete_el(El, N, Lst):-
    %length(Lst,N),
   % maplist(unifica(El), Lst).


%substitui_maiores_N(N, Subst, L1, L2):-
    %maplist(substitui_maiores_N_aux(N,Subst), L1, L2).

%substitui_maiores_N_aux(N, _, El, El):-
    %El =< N.

%substitui_maiores_N_aux(N, Subst, El, Subst):-
    %El > N.


% pratica#2
% divisor(D,N):-
%    mod(N,D) =:= 0.


% soma_digitos_recursiva(N, N):-
 %   (N < 10).

%soma_digitos_recursiva(N, S):-
  %  N >= 10,
 %   Elem is N mod 10, % mod(N, 10)
   % Novo_n is N // 10,
  %  soma_digitos_recursiva(Novo_n, Soma_recursiva),
  %  S is (Soma_recursiva + Elem).
% -----------------------------
% soma_digitos_iter(N, S):-
%    soma_digitos_iter(N, 0, S).

%soma_digitos_iter(_, Res, Res).

% soma_digitos_iter(N, Res, S):-
  %  N_aux is N // 10,
 %   Elem is N mod 10,
 %   NovoRes is Res + Elem,
  %  soma_digitos_iter(N_aux, NovoRes, S).

  %-----------------------------

% inverte(N, Inv):- 
   % inverte(N, Inv, 0).

% inverte(0, Inv, Inv).

% inverte(N, Inv, Aux):-
  %  N > 0,
  %  Dig_direirta is N mod 10,
 %   N_sem_dig_direita is N // 10,
  %  N_aux is Aux*10 + Dig_direirta,
 %   inverte(N_sem_dig_direita, Inv, N_aux).

%---------------------
% -------------------------------------------------------------

% corte !

% p(X, Y):- q(X), !, r(Y).
% vai correr so um q(X), e todos os r(Y).





