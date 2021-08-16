:- use_module(library(lists)).

:- dynamic(worklist/1).
% worklist([process_events, render, tick]).
worklist([process_events, tick]).

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

this_tick(This) :- q(system, tick, val, This).

process_events(P) :-
  this_tick(T),
  event(T, Id, Type),
  event_handler(Id, Type, H),
  call(H, P).
process_events([]).

write_event(Id, Type) :- this_tick(N), assertz(event(N, Id, Type)).

% render :-
%   index_html(Html),
%   % !,
%   this_tick(N),
%   asserta(rendering(N, Html)).

work(Reactor) :-
  call(Reactor, Patch),
  % commit now to make writes visible to later listeners
  do_commit(Reactor, Patch).

% process_tick :-
process_tick(Html) :-
  worklist(L),
  maplist(work, L),
  todos(Html).
  % this_tick(N),
  % rendering(N, Html).

% initialize zeroth tick - if I just use content hashing for IRIs I can move
% init/1 back to the first reactor
quint('system@0#init/', tick, val, 0, true).
quint('system@0#init', 'system@0#init', a, commit, true).
quint('system@0#init', 'system@0#init', data, 'system@0#init/', true).
quint(heads, system, head, 'system@0#init', true).

% bench(0).
% bench(N) :-
%   time(render),
%   Next is N - 1,
%   bench(Next).
