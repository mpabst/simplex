todos(h(div, [], [ h(div, [], [N]), B, h(ul, [], Todos) ])) :-
  this_tick(N),
  button(B),
  findall(L, q(todos, subj, label, L), Labels),
  todo(Labels, Todos).

button(h(button, [id('create-todo'), on(click)], ['new todo'])).

todo([], []).
todo([L|In], [h(li, [], [L])|Out]) :- todo(In, Out).

create_todo([quint(todos, subj, label, N, true)]) :- this_tick(N).

event_handler('create-todo', click, create_todo).
