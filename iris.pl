:- use_module(library(lists)).

% special case init/1, because it's what increments the tick.
meta_iri_chars(Graph, init, IRIChars) :-
  next_tick(N),
  meta_iri_chars_(N, Graph, init, IRIChars).
meta_iri_chars(Graph, Reactor, IRIChars) :-
  Reactor \= init,
  this_tick(N),
  meta_iri_chars_(N, Graph, Reactor, IRIChars).

% tick_graph_iri(sym+, sym+, sym-) is det
meta_iri_chars_(N, Graph, Reactor, IRIChars) :-
  number_chars(N, TickChars),
  atom_chars(Graph, GraphChars),
  atom_chars(Reactor, ReactorChars),
  % why can't I just inline these as strings into L?
  atom_chars('@', At),
  atom_chars('#', Hash),
  L = [GraphChars, At, TickChars, Hash, ReactorChars],
  append(L, IRIChars).

meta_iri(Graph, Reactor, IRI) :-
  meta_iri_chars(Graph, Reactor, IRIChars),
  atom_chars(IRI, IRIChars).

data_iri(Graph, Reactor, IRI) :-
  meta_iri_chars(Graph, Reactor, Meta),
  atom_chars('/', Slash),
  L = [Meta, Slash],
  append(L, Chars),
  atom_chars(IRI, Chars).
