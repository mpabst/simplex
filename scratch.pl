:- dynamic(commit/1).
commit(0).

:- dynamic(head/1).
head(0).

update :-
  head(H),
  I is H + 1,
  assertz(commit(I)),
  assertz(head(I)),
  retract(head(H)).
