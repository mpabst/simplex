:- dynamic(quint/5).

% it'd be nice if these query/test rules could somehow be turned into data
% constructors as well. i guess we can always just dig up the rule body from
% the DB? what about variable bindings?
data(Meta, Data) :- quint(Meta, Meta, data, Data).

parent(Meta, Parent) :- quint(Meta, Meta, parent, Parent, true).

head(Graph, Meta) :- quint(heads, Graph, head, Meta, true).

search_history(Meta, S, P, O) :-
  data(Meta, Data),
  quint(Data, S, P, O, false),
  fail.
search_history(Meta, S, P, O) :- 
  data(Meta, Data),
  quint(Data, S, P, O, true).
search_history(Meta, S, P, O) :-
  parent(Meta, Parent),
  search_history(Parent, S, P, O).

q(G, S, P, O) :- head(G, Meta), search_history(Meta, S, P, O).
