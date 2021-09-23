:- use_module(library(apply)).
:- use_module(library(lists)).

:- dynamic(worklist/1).
% worklist([process_events, render, tick]).
worklist([process_events, create_todo, tick]).

foo([quint(bar, baz, quux, M, false), quint(bar, baz, quux, N, true)]) :-
  this_tick(N),
  M is N - 1.

% event(Tick, Id, Type)
:- dynamic(event/3).

% rendering(Tick, Html)
:- dynamic(rendering/2).

tick([
  quint(system, tick, val, T, false),
  quint(system, tick, val, S, true)
]) :-
  this_tick(T),
  S is T + 1.

this_tick(This) :-
  findall(O, quint(_, tick, val, O, true), L),
  max_list(L, This).

process_events(P) :-
  this_tick(T),
  event(T, Id, Type),
  event_handler(Id, Type, H),
  call(H, P).
process_events([]).

write_event(Id, Type) :- this_tick(N), assertz(event(N, Id, Type)).

work(Reactor) :-
  call(Reactor, Patch),
  % commit now to make writes visible to later listeners
  do_commit(Reactor, Patch).

process_tick(Html) :-
  worklist(L),
  maplist(work, L),
  todos(Html).

quint('system@0#init/', tick, val, 1, true).
quint('system@0#init', 'system@0#init', a, commit, true).
quint('system@0#init', 'system@0#init', data, 'system@0#init/', true).
quint(heads, system, head, 'system@0#init', true).

:- use_module(library(statistics)).

bench(0).
bench(N) :-
  time(process_tick(_)),
  Next is N - 1,
  bench(Next).
