:- use_module(library(lists)).

index_html(Html) :-
  G = graph,
  findall(O, (
    quad(G, S, a, todo),
    quad(G, S, label, O) 
  ), L),
  this_tick(N),
  todos_html(L, Todos),
  button(B),
  Html = h(div, [], [ h(div, [], [N]), B, h(ul, [], Todos) ]).

button(B) :-
  B = h(button, [id('create-todo'), on(click)], ['new todo']).

todos_html([], []).
todos_html([HI|TI], [HO|TO]) :-
  HO = h(li, [], [HI]),
  todos_html(TI, TO).

:- dynamic(quad/4).
quad(graph, subj, a, todo).
quad(graph, subj, label, 'something').
quad(graph, subj, label, 'something else').

quad(graph, subj2, a, todo).
quad(graph, subj2, label, 'subj2').

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

insert_todo(A, R) :-
  this_tick(N),
  A = [quad(graph, subj, label, N)],
  R = [].

event_handler('create-todo', click, insert_todo).

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
