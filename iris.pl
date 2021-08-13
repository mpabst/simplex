:- use_module(library(lists)).

% tick_graph_iri(sym+, sym+, sym-) is det
meta_iri_chars(Graph, Reactor, IRIChars) :-
  this_tick(N),
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
  meta_iri_chars(Graph, Reactor, Met),
  atom_chars('/', Slash),
  L = [Meta, Slash],
  append(L, Chars),
  atom_chars(IRI, Chars).
