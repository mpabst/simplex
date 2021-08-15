:- dynamic(quint/5).

% it'd be nice if these query/test rules could somehow be turned into data
% constructors as well. i guess we can always just dig up the rule body from
% the DB? what about variable bindings?
data(Meta, Data) :- quint(Meta, Meta, data, Data, true).

parent(Meta, Parent) :- quint(Meta, Meta, parent, Parent, true).

head(Graph, Meta) :- quint(heads, Graph, head, Meta, true).

retractions(Meta, S, P, O) :- data(Meta, Data), quint(Data, S, P, O, false).

assertions(Meta, S, P, O) :- data(Meta, Data), quint(Data, S, P, O, true).

asserted(Meta, S, P, O) :- retractions(Meta, S, P, O), fail.
asserted(Meta, S, P, O) :- assertions(Meta, S, P, O).
asserted(Meta, S, P, O) :-
  data(Meta, Data),
  % is there a way to avoid restating this 'guard'?
  \+quint(Data, S, P, O, _),
  parent(Meta, Parent),
  asserted(Parent, S, P, O).

q(G, S, P, O) :- head(G, Meta), asserted(Meta, S, P, O).
