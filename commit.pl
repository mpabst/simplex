:- use_module(library(lists)).

collect_graphs(Patch, Graphs) :-
  collect_graphs_(Patch, [], Unsorted),
  sort(Unsorted, Graphs).

collect_graphs_([], Acc, Graphs) :- Acc = Graphs.
collect_graphs_([quint(G, _, _, _, _)|Patch], Acc, Graphs) :-
  collect_graphs_(Patch, [G|Acc], Graphs).

commit_meta(Reactor, Graphs, Assertions, Retractions) :-
  commit_meta_(Reactor, Graphs, [], Assertions, [], Retractions).

commit_meta_(_, [], AAcc, AOut, RAcc, ROut) :-
  AAcc = AOut,
  RAcc = ROut.
commit_meta_(Reactor, [G|Graphs], AAcc, AOut, RAcc, ROut) :-
  declare_commit(Reactor, G, CommitA),
  declare_parent(Reactor, G, ParentA),
  declare_ref(Reactor, G, RefA, RefR),
  append([AAcc, CommitA, ParentA, RefA], ANext),
  append([RAcc, RefR], RNext),
  commit_meta_(Reactor, Graphs, ANext, AOut, RNext, ROut).

declare_commit(Reactor, Graph, Assertions) :-
  meta_iri(Graph, Reactor, Meta),
  data_iri(Graph, Reactor, Data),
  Assertions = [
    quint(Meta, Meta, a, commit, true),
    quint(Meta, Meta, data, Data, true)
  ].

declare_parent(_, Graph, Assertions) :-
  \+head(Graph, _),
  Assertions = [].
declare_parent(Reactor, Graph, Assertions) :-
  head(Graph, Head),
  meta_iri(Graph, Reactor, Meta),
  Assertions = [quint(Meta, Meta, parent, Head, true)].

% TODO: maintain a patch log for the heads graph; we should only have one IRI of
% mutable state, the current head of the heads graph.
% no parent
declare_ref(Reactor, Graph, Assertions, Retractions) :-
  \+head(Graph, _),
  meta_iri(Graph, Reactor, Meta),
  Assertions = [quint(heads, Graph, head, Meta, true)],
  Retractions = [].
% has parent
declare_ref(Reactor, Graph, Assertions, Retractions) :-
  head(Graph, Head),
  meta_iri(Graph, Reactor, Meta),
  Assertions = [quint(heads, Graph, head, Meta, true)],
  % just retract the old ref for now
  Retractions = [quint(heads, Graph, head, Head, true)].

% TODO: omit inapplicable quints? retractions of currently unasserted data, m.m.
do_commit(Reactor, Patch) :-
  full_commit(Reactor, Patch, Assertions, Retractions),
  % why is this cut needed?
  !,
  maplist(retract, Retractions),
  maplist(assertz, Assertions).

full_commit(Reactor, Patch, Assertions, Retractions) :-
  rewrite_graph_terms(Reactor, Patch, Rewritten),
  collect_graphs(Patch, Graphs),
  commit_meta(Reactor, Graphs, MetaAssertions, Retractions),
  append([Rewritten, MetaAssertions], Assertions).

rewrite_graph_terms(Reactor, Patch, Rewritten) :-
  rewrite_graph_terms_(Reactor, Patch, [], Rewritten).

rewrite_graph_terms_(_, [], Acc, Rewritten) :- Acc = Rewritten.
rewrite_graph_terms_(Reactor, [quint(G, S, P, O, V)|Patch], Acc, Rewritten) :-
  data_iri(G, Reactor, Data),
  RAcc = [quint(Data, S, P, O, V)|Acc],
  rewrite_graph_terms_(Reactor, Patch, RAcc, Rewritten).
