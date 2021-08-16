:- dynamic(quint/5).

% it'd be nice if these query/test rules could somehow be turned into data
% constructors as well. i guess we can always just dig up the rule body from
% the DB? what about variable bindings?
data(Meta, Data) :- quint(Meta, Meta, data, Data, true).

parent(Meta, Parent) :- quint(Meta, Meta, parent, Parent, true).

head(Graph, Meta) :- quint(heads, Graph, head, Meta, true).

asserted(quad(G, S, P, O), _, Data, Retractions) :-
  quint(Data, S, P, O, true),
  \+member(quad(G, S, P, O), Retractions).
asserted(quad(G, S, P, O), Meta, Data, Retractions) :-
  parent(Meta, Parent),
  data(Parent, PData),
  findall(quad(G, S, P, O), quint(Data, S, P, O, false), R),
  append([R, Retractions], NextR),
  asserted(quad(G, S, P, O), Parent, PData, NextR).

q(G, S, P, O) :- head(G, H), data(H, D), asserted(quad(G, S, P, O), H, D, []).
