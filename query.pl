:- dynamic(quint/5).

% it'd be nice if these query/test rules could somehow be turned into data
% constructors as well. i guess we can always just dig up the rule body from
% the DB? what about variable bindings?
data(Meta, Data) :- quint(Meta, Meta, data, Data, true).

parent(Meta, Parent) :- quint(Meta, Meta, parent, Parent, true).

head(Graph, Meta) :- quint(heads, Graph, head, Meta, true).

retractions(Data, S, P, O) :- quint(Data, S, P, O, false).

assertions(Data, S, P, O) :- quint(Data, S, P, O, true).

asserted(Meta, Data, S, P, O) :-
  retractions(Data, S, P, O), fail
  ;
  assertions(Data, S, P, O)
  ;
  \+quint(Data, S, P, O, _),
  parent(Meta, Parent),
  data(Parent, PData),
  asserted(Parent, PData, S, P, O).

q(G, S, P, O) :- head(G, Meta), data(Meta, Data), asserted(Meta, Data, S, P, O).
