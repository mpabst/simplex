% :- use_module(library(dcg/basics)).

% null event stub to make rusty-wam happy, cause ig the dynamic decl isn't
% enough
event(-1, null, nada).
event_handler(nothing, nada, no_way).
no_way([]).

append([], []).
append([[]|T], A) :- append(T, A).
append([[H|Ht]|T], [H|A]) :- append([Ht|T], A).

% just init to 0 for now, we only use this for this_tick/1
max_list(L, M) :- max_list(L, 0, M).

max_list([], M, M).
max_list([H|T], Ac, M) :- H > Ac, max_list(T, H, M).
max_list([H|T], Ac, M) :- H =< Ac, max_list(T, Ac, M).
