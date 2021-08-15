:- use_module(library(lists)).

commit_meta(Reactor, Graphs, Extra) :- commit_meta(Reactor, Graphs, [], Extra).

commit_meta(_, [], EAcc, Extra) :- append(EAcc, Extra). % flatten
commit_meta(Reactor, [G|Graphs], EAcc, Extra) :-
  meta_iri(G, Reactor, Meta),
  data_iri(G, Reactor, Data),
  declare_commit(G, Meta, Data, E),
  commit_meta(Reactor, Graphs, [E|EAcc], Extra).

declare_commit(Graph, Meta, Data, Extra) :-
  (head(Graph, Head) ->
    (Parent = [
      quint(Meta, Meta, parent, Head, true),
      % retract old head ref
      quint(heads, Graph, head, Head, false)
    ])
  ; (Parent = [])),

  Extra = [
    quint(heads, Graph, head, Meta, true),
    quint(Meta, Meta, a, commit, true),
    quint(Meta, Meta, data, Data, true)
  | Parent ].

% TODO: omit inapplicable quints? retractions of currently unasserted data, m.m.
do_commit(Reactor, Patch) :-
  full_commit(Reactor, Patch, Processed, Extra),
  % why is this cut needed? probably because of the side effects after it:
  % prolog does the writes, then backtracks to full_commit, which now gives a
  % different result. but i don't really know.
  !,
  maplist(assertz, Processed),
  maplist(do_write, Extra).

do_write(quint(G, S, P, O, true)) :- assertz(quint(G, S, P, O, true)).
do_write(quint(G, S, P, O, false)) :- retract(quint(G, S, P, O, true)).

full_commit(Reactor, Patch, Processed, Extra) :-
  process_patch_data(Reactor, Patch, Processed, Graphs),
  % !, for optimization reasons? since we know the predicate is deterministic
  commit_meta(Reactor, Graphs, Extra).

process_patch_data(Reactor, Patch, Processed, Graphs) :-
  process_patch_data(Reactor, Patch, [], Processed, [], Graphs).

process_patch_data(_, [], PAcc, Processed, GAcc, Graphs) :-
  PAcc = Processed,
  sort(GAcc, Graphs).
process_patch_data(Reactor, [quint(G, S, P, O, V)|Patch], PAcc, Processed, GAcc, Graphs) :-
  data_iri(G, Reactor, Data),
  process_patch_data(Reactor, Patch, [quint(Data, S, P, O, V)|PAcc], Processed, [G|GAcc], Graphs).
