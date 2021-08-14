:- use_module(library(lists)).

write_commit_decl(Reactor, Graph) :-
  meta_iri(Graph, Reactor, Meta),
  assertz(quint(Meta, Meta, a, commit, true)),
  data_iri(Graph, Reactor, Data),
  assertz(quint(Meta, Meta, data, Data, true)).

% TODO: maintain a patch log for the heads graph; we should only have one IRI of
% mutable state, the current head of the heads graph.
% has parent
update_head(Reactor, Graph) :-
  meta_iri(Graph, Reactor, Meta),
  head(Graph, Head),
  assertz(quint(Meta, Meta, parent, Head, true)),
  assertz(quint(heads, Graph, head, Meta, true)),
  % just retract the old ref for now
  retract(quint(heads, Graph, head, Head, true)).
% no parent
update_head(Reactor, Graph) :-
  meta_iri(Graph, Reactor, Meta),
  assertz(quint(heads, Graph, head, Meta, true)).

write_patch_data(Reactor, Patch, Graphs) :-
  write_patch_data_(Reactor, Patch, [], Graphs).

% TODO: omit inapplicable quints? retractions of currently unasserted data, m.m.
write_patch_data_(_, [], Accum, Graphs) :- Accum = Graphs.
write_patch_data_(Reactor, [quint(G, S, P, O, V)|Patch], Accum, Graphs) :-
  data_iri(G, Reactor, Data),
  assertz(quint(Data, S, P, O, V)),
  write_patch_data_(Reactor, Patch, [G|Accum], Graphs).

do_commit(Reactor, Patch) :-
  write_patch_data(Reactor, Patch, Graphs),
  sort(Graphs, Sorted),
  maplist(write_commit_decl(Reactor), Sorted),
  maplist(update_head(Reactor), Sorted).
