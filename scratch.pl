% :- dynamic(commit/1).
% commit(0).

% :- dynamic(head/1).
% head(0).

% update :-
%   head(H),
%   I is H + 1,
%   assertz(commit(I)),
%   assertz(head(I)),
%   retract(head(H)).

% worklist([init, foo]).

% foo(P) :-
%   this_tick(N),
%   M is N - 1,
%   P = [quint(bar, baz, quux, M, false), quint(bar, baz, quux, N, true)].


% foo(O) :- foo(0, O).
% foo(N, O) :- M is N + 1, (O = M ; foo(M, O)).

:- discontiguous tru/3.

tru(0, foo, f).
tru(0, bar, b).

fals(1, foo, f).
tru(1, quux, q).

retracted(F, S, fals(_, F, S)).

as_of(N, First, Second, Data) :- as_of(N, First, Second, [], [], Data).

as_of(-1, _, _, A, _, A) :- !.
as_of(N, First, Second, Ass, Ret, Data) :-
  findall(tru(N, First, Second), (
    tru(N, First, Second),
    include(retracted(First, Second), Ret, [])
  ), CurrA),
  append([CurrA, Ass], NextAss),
  findall(fals(N, First, Second), fals(N, First, Second), CurrR),
  append([CurrR, Ret], NextRet),
  M is N - 1,
  as_of(M, First, Second, NextAss, NextRet, Data).
