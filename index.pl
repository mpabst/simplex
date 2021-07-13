:- use_module(library(dom)).
:- use_module(library(lists)).

index_html(UL) :-
  G = graph,
  create(ul, UL),
  add_button(UL),
  quad(G, S, a, todo),
  quad(G, S, label, O),
  todos_html(UL, O).

add_button(UL) :-
  create(button, B),
  bind(B, click, _, write_event(button, click)),
  append_child(UL, B).

todos_html(_, []).
todos_html(UL, [L|Labels]) :-
  create(li, LI),
  html(LI, L),
  append_child(UL, LI),
  todos_html(UL, Labels).

flatten([]) --> [], !.
flatten([A|T]) --> { \+is_list(A) }, [A], !, flatten(T).
flatten([A|T]) --> flatten(A), flatten(T).

:- dynamic(quad/4).
quad(graph, subj, a, todo).
quad(graph, subj, label, "something").
quad(graph, subj, label, "something else").

quad(graph, subj2, a, todo).
quad(graph, subj2, label, "subj2").

:- dynamic(to_assert/1).
:- dynamic(to_retract/1).

:- dynamic(worklist/1).
% TODO: make all reactors /1, and have them emit the next write;
% special case commit?
worklist([commit, process_events, render, write_next_tick]).

:- dynamic(tick/1).
tick(0).

% event(Tick, Id, Type)
:- dynamic(event/3).

schedule_write([]).
schedule_write([H|T]) :-
  H = to_assert(A),
  assertz(to_assert(A)),
  schedule_write(T).
schedule_write([H|T]) :-
  H = to_retract(R),
  assertz(to_retract(R)),
  schedule_write(T).

next_tick(N) :-
  this_tick(T),
  N is T + 1.

% fold into commit/0?
write_next_tick :-
  next_tick(N),
  schedule_write([to_assert(tick(N))]).

commit :-
  to_assert(A),
  asserta(A),
  retract(to_assert(A)).
commit :-
  to_retract(R),
  retract(R),
  retract(to_retract(R)).

this_tick(This) :-
  findall(Tick, tick(Tick), L),
  max_list(L, This).

insert_todo :-
  next_tick(N),
  schedule_write([to_assert(quad(graph, subj, label, N))]).

event_handler(button, click, insert_todo).

process_events :-
  this_tick(T),
  event(T, Id, Type),
  event_handler(Id, Type, H),
  call(H).

write_event(Id, Type) :-
  next_tick(N),
  schedule_write([to_assert(event(N, Id, Type))]),
  process_tick.

render :-
  index_html(Rendered),
  get_by_id(root, Root),
  html(Root, Rendered).

work([]).
work([P|T]) :-
  call(P),
  work(T).

process_tick :-
  worklist(L),
  work(L).

bench(0).
bench(N) :-
  time(render),
  Next is N - 1,
  bench(Next).
