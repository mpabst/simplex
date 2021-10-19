:- use_module(library(terminus_store)).

objects(0, value("0")).
objects(N, value(C)) :- N > 0, number_string(N, C).
objects(N, C) :- N > 0, M is N - 1, objects(M, C).

% foo(bar, "Subject", "Predicate", O) :- print(O).

write_patch :-
  open_directory_store("testdir", Store),
  open_named_graph(Store, "foo", DB),
  open_write(DB, Builder),
  findall(O, objects(100, O), L),
  maplist(nb_add_triple(Builder, "Subject", "Predicate"), L),
  % nb_add_triple(Builder, "Subject", "Predicate", O),
  nb_commit(Builder, Layer),
  nb_set_head(DB, Layer).


open_directory_store("testdir", Store),
open_named_graph(Store, "foo", DB),
head(DB, Layer),
triple(Layer, Subject, Predicate, Object).
