:- use_module(library(lists)).

:- dynamic(quad/4).
quad(Commit, S, P, O) :-
  assertions(Commit, A),
  once(quad(A, S, P, O)).

quad(Commit, S, P, O) :-
  retractions(Commit, R),
  once((quad(R, S, P, O), fail)).

quad(Commit, S, P, O) :-
  parent(Commit, Pa),
  once(quad(Pa, S, P, O)).

quad(G, S, P, O) :-
  head(G, Commit),
  quad(Commit, S, P, O).

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
  write_assertions([event(N, Id, Type)]).

render(A, R) :-
  index_html(Html),
  this_tick(N),
  A = [rendering(N, Html)],
  R = [].

write_assertions([]).
write_assertions([H|T]) :-
  assertz(H),
  write_assertions(T).

write_retractions([]).
write_retractions([H|T]) :-
  retract(H),
  write_retractions(T).

work([]).
work([H|T]) :-
  call(H, A, R),
  % call these now to make writes visible to later listeners
  write_assertions(A),
  write_retractions(R),
  work(T).

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
