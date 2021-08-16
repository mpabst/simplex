todos(Html) :-
  this_tick(N),
  button(B),
  findall(L, q(todos, subj, label, L), Labels),
  todo(Labels, Todos),
  Html = h(div, [], [ h(div, [], [N]), B, h(ul, [], Todos) ]).

button(B) :-
  B = h(button, [id('create-todo'), on(click)], ['new todo']).

todo([], []).
todo([quad(todos, subj, label, L)|In], [h(li, [], [L])|Out]) :- todo(In, Out).

create_todo(P) :-
  this_tick(N),
  P = [quint(todos, subj, label, N, true)].

event_handler('create-todo', click, create_todo).
