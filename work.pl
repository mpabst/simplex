:- use_module(library(lists)).

:- dynamic(worklist/1).
% worklist([init, process_events, render]).
worklist([init, foo]).

foo(P) :-
  this_tick(N),
  M is N - 1,
  P = [quint(bar, baz, quux, M, false), quint(bar, baz, quux, N, true)].

% event(Tick, Id, Type)
:- dynamic(event/3).

% rendering(Tick, Html)
:- dynamic(rendering/2).

init(P) :-
  next_tick(N),
  M is N - 1,
  P = [
    quint(system, tick, val, M, false),
    quint(system, tick, val, N, true)
  ].

next_tick(N) :-
  this_tick(T),
  N is T + 1.

this_tick(This) :- q(system, tick, val, Latest) -> This = Latest ; This = 0.

process_events(P) :-
  this_tick(T),
  event(T, Id, Type),
  event_handler(Id, Type, H),
  call(H, P).
process_events(P) :- P = [].

write_event(Id, Type) :-
  next_tick(N),
  assertz(event(N, Id, Type)).

render(P) :-
  index_html(Html),
  this_tick(N),
  P = [rendering(N, Html)].

work(Reactor) :-
  call(Reactor, Patch),
  % commit now to make writes visible to later listeners
  do_commit(Reactor, Patch).

process_tick :-
% process_tick(Html) :-
  worklist(L),
  maplist(work, L).
  % this_tick(N),
  % rendering(N, Html).

% bench(0).
% bench(N) :-
%   time(render),
%   Next is N - 1,
%   bench(Next).
