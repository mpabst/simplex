:- use_module(library(lists)).

:- dynamic(quad/4).
% quad(Commit, S, P, O) :-
%   assertions(Commit, A),
%   once(quad(A, S, P, O)).

% quad(Commit, S, P, O) :-
%   retractions(Commit, R),
%   once((quad(R, S, P, O), fail)).

% quad(Commit, S, P, O) :-
%   parent(Commit, Pa),
%   once(quad(Pa, S, P, O)).

% quad(G, S, P, O) :-
%   head(G, Commit),
%   quad(Commit, S, P, O).

assertions(Commit, A) :-
  quad(Commit, Commit, assertions, A).

retractions(Commit, R) :-
  quad(Commit, Commit, retractions, R).

parent(Commit, P) :-
  quad(Commit, Commit, parent, P).

head(G, Commit) :-
  quad(heads, G, head, Commit).

:- dynamic(worklist/1).
worklist([init, process_events, render]).

:- dynamic(tick/1).
tick(0).

% event(Tick, Id, Type)
:- dynamic(event/3).

% rendering(Tick, Html)
:- dynamic(rendering/2).

next_tick(N) :-
  this_tick(T),
  N is T + 1.

init(A, R) :-
  next_tick(N),
  A = [tick(N)],
  R = [].

this_tick(This) :-
  findall(Tick, tick(Tick), L),
  max_list(L, This).

process_events(A, R) :-
  (this_tick(T), event(T, Id, Type)) -> 
    event_handler(Id, Type, H),
    call(H, A, R)
    ;
    A = [], R = [].

write_event(Id, Type) :-
  next_tick(N),
  assertz(event(N, Id, Type)).

render(A, R) :-
  index_html(Html),
  this_tick(N),
  A = [rendering(N, Html)],
  R = [].

% tick_graph_iri(sym+, char[]+, sym+, sym-) is det
tick_graph_iri(Graph, ReactorName, Type, IRI) :-
  this_tick(Tick),
  number_chars(Tick, TickChars),
  atom_chars(Graph, GraphChars),
  L = [GraphChars, "@", TickChars, "#", ReactorName, "/", Type],
  reverse(L, R),
  append(R, A),
  atom_chars(IRI, A).

write_commit_decl(ReactorName, Graph) :-
  tick_graph_iri(Graph, ReactorName, "commit", Commit),
  assertz(quad(Commit, Commit, a, commit)).

write_assertions_ref(ReactorName, Graph) :-
  % write("assertions ref"),
  tick_graph_iri(Graph, ReactorName, "commit", Commit),
  tick_graph_iri(Graph, ReactorName, "assert", Assertions),
  assertz(quad(Commit, Commit, assertions, Assertions)).

write_retractions_ref(ReactorName, Graph) :-
  tick_graph_iri(Graph, ReactorName, "commit", Commit),
  tick_graph_iri(Graph, ReactorName, "retract", Retractions),
  assertz(quad(Commit, Commit, retractions, Retractions)).

% case where there is already a head
update_head(ReactorName, Graph) :-
  tick_graph_iri(Graph, ReactorName, "commit", Commit),
  head(Graph, Head),
  assertz(quad(Commit, Commit, parent, Head)),
  assertz(quad(heads, Graph, head, Commit)),
  retract(quad(heads, Graph, head, Head)).

% case where there isn't
update_head(ReactorName, Graph) :-
  tick_graph_iri(Graph, ReactorName, "commit", Commit),
  assertz(quad(heads, Graph, head, Commit)).

write_retractions(_, [], []).
write_retractions(ReactorName, [quad(G, S, P, O)|Retractions], Graphs) :-
  tick_graph_iri(G, ReactorName, "retract", RetractionsGraph),
  assertz(quad(RetractionsGraph, S, P, O)),
  Graphs = [G|RGraphs],
  write_retractions(ReactorName, Retractions, RGraphs).

write_assertions(_, [], []).
write_assertions(ReactorName, [quad(G, S, P, O)|Assertions], Graphs)  :-
  tick_graph_iri(G, ReactorName, "assert", AssertionsGraph),
  assertz(quad(AssertionsGraph, S, P, O)),
  Graphs = [G|RGraphs],
  write_assertions(ReactorName, Assertions, RGraphs).

do_commit(ReactorName, Assertions, Retractions) :-
  write_assertions(ReactorName, Assertions, AssertionGraphs),
  write_retractions(ReactorName, Retractions, RetractionGraphs),
  maplist(write_assertions_ref(ReactorName), AssertionGraphs),
  maplist(write_retractions_ref(ReactorName), RetractionGraphs),
  append(AssertionGraphs, RetractionGraphs, Graphs),
  sort(Graphs, Sorted),
  maplist(write_commit(ReactorName), Sorted),
  maplist(update_head(ReactorName), Sorted).

work(Reactor) :-
  call(Reactor, Assertions, Retractions),
  atom_chars(Reactor, ReactorName),
  % commit now to make writes visible to later listeners
  do_commit(ReactorName, Assertions, Retractions).

process_tick(Html) :-
  worklist(L),
  maplist(work, L),
  this_tick(N),
  rendering(N, Html).

bench(0).
bench(N) :-
  time(render),
  Next is N - 1,
  bench(Next).
