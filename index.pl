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

% tick_graph_iri(sym+, sym+, sym+, sym-) is det
tick_graph_iri(G, Type, Reactor, Name) :-
  this_tick(N),
  number_chars(N, NC),
  atom_chars(G, Gs),
  append([Gs, "/", Type, "@", NC, "#", Reactor], String),
  atom_chars(Name, String).

% class decl
commit(N, G) :-
  tick_graph_iri(G, N, "commit", T),
  assertz(quad(T, T, a, commit)).

% assertions ref
commit(N, G) :-
  tick_graph_iri(G, N, "commit", C),
  tick_graph_iri(G, N, "assert", A),
  once(quad(A, _, _, _)),
  assertz(quad(C, C, assertions, A)).

% retractions ref
commit(N, G) :-
  tick_graph_iri(G, N, "commit", C),
  tick_graph_iri(G, N, "retract", R),
  once(quad(R, _, _, _)),
  assertz(quad(C, C, retractions, R)).

% parent ref, update head
commit(N, G) :-
  tick_graph_iri(G, N, "commit", C),
  head(G, H) -> (
    assertz(quad(C, C, parent, H)),
  	assertz(quad(heads, G, head, C)),
  	retract(quad(heads, G, head, H))
  ) ; assertz(quad(heads, G, head, C)).

commit(_, [], []).

commit(N, A, R) :-
  sort(A, AS),
  sort(R, RS),
  commit(N, AS, RS, []).

% commits proper, after A & R are both done
commit(Reactor, [], [], G) :-
  sort(G, S),
  maplist(commit(Reactor), S).

% retractions, after assertions
commit(Reactor, [], [quad(G, S, P, O)|Retractions], Graphs) :-
  tick_graph_iri(G, "retract", Reactor, TickGraph),
  assertz(quad(TickGraph, S, P, O)),
  commit(Reactor, [], Retractions, [G|Graphs]).

% assertions
% commit(string+, [quad/4]+, [quad/4]+, [string]+) is undef
commit(Reactor, [quad(G, S, P, O)|Assertions], Retractions, Graphs) :-
  tick_graph_iri(G, "assert", Reactor, TickGraph),
  assertz(quad(TickGraph, S, P, O)),
  commit(Reactor, Assertions, Retractions, [G|Graphs]).

work([]).
work([Reactor|Tail]) :-
  call(Reactor, Assertions, Retractions),
  atom_chars(Reactor, ReactorName),
  % commit now to make writes visible to later listeners
  commit(ReactorName, Assertions, Retractions),
  work(Tail).

process_tick(Html) :-
  worklist(L),
  work(L),
  this_tick(N),
  rendering(N, Html).

bench(0).
bench(N) :-
  time(render),
  Next is N - 1,
  bench(Next).
