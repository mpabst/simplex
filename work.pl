:- use_module(library(lists)).

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

init(P) :-
  next_tick(N),
  P = [tick(N)].

this_tick(This) :-
  findall(Tick, tick(Tick), L),
  max_list(L, This).

process_events(P) :-
  (this_tick(T), event(T, Id, Type)) -> (
    event_handler(Id, Type, H),
    call(H, P)
  ) ;
    P = [].

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

process_tick(Html) :-
  worklist(L),
  maplist(work, L),
  this_tick(N),
  rendering(N, Html).

% bench(0).
% bench(N) :-
%   time(render),
%   Next is N - 1,
%   bench(Next).
