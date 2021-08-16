:- dynamic(quint/5).

% it'd be nice if these query/test rules could somehow be turned into data
% constructors as well. i guess we can always just dig up the rule body from
% the DB? what about variable bindings?
data(Meta, Data) :- quint(Meta, Meta, data, Data, true).

parent(Meta, Parent) :- quint(Meta, Meta, parent, Parent, true).

ancestor(Meta, Meta).
ancestor(Meta, Ancestor) :- parent(Meta, P), ancestor(P, Ancestor).

head(Graph, Meta) :- quint(heads, Graph, head, Meta, true).

retractions(Data, S, P, O) :- quint(Data, S, P, O, false).

assertions(Data, S, P, O) :- quint(Data, S, P, O, true).

% convert to test predicate which short-circuits? how often will i use that?
% asserted(_, Data, S, P, O) :- assertions(Data, S, P, O).
% asserted(_, Data, S, P, O) :- retractions(Data, S, P, O), fail.
% asserted(Meta, _, S, P, O) :-
%   parent(Meta, Parent),
%   data(Parent, PData),
%   asserted(Parent, PData, S, P, O).

collect(quad(G, S, P, O), Data, Sign, Append, Exclude, Out) :-
  Q = quad(G, S, P, O),
  findall(Q, (quint(Data, S, P, O, Sign), \+member(Q, Exclude)), Current),
  append([Current, Append], A),
  sort(A, Out).

% assumes patches are well-formed and don't try to simultaneously assert and
% retract a quad
asserted(Quad, Meta, Asserted, Retracted, L) :-
  data(Meta, Data),
  collect(Quad, Data, true, Asserted, Retracted, NextA),
  collect(Quad, Data, false, Retracted, Asserted, NextR),
  ( parent(Meta, Parent) ->
      asserted(Quad, Parent, NextA, NextR, L)
      ;
      NextA = L ).

quads(G, S, P, O, Quads) :-
  head(G, Meta),
  findall(L, asserted(quad(G, S, P, O), Meta, [], [], L), LL),
  append(LL, Quads).
