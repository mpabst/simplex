todos(Html) :-
  this_tick(N),
  button(B),
  G = graph,
  data(G, L),
  todo(L, Todos),
  Html = h(div, [], [ h(div, [], [N]), B, h(ul, [], Todos) ]).

button(B) :-
  B = h(button, [id('create-todo'), on(click)], ['new todo']).

todo([], []).
todo([HI|TI], [HO|TO]) :-
  HO = h(li, [], [HI]),
  todo(TI, TO).

data(G, L) :-
  findall(O, (
    quad(G, S, a, todo),
    quad(G, S, label, O) 
  ), L).

create_todo(A, R) :-
  this_tick(N),
  A = [quad(graph, subj, label, N)],
  R = [].

event_handler('create-todo', click, create_todo).

quad(assert0, subj, a, todo).
quad(assert0, subj, label, 'something').
quad(assert0, subj, label, 'something else').

quad(assert0, subj2, a, todo).
quad(assert0, subj2, label, 'subj2').

quad(commit0, commit0, a, commit).
quad(commit0, commit0, assertions, assert0).

quad(heads, todos, head, commit0).
