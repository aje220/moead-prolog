


divide_by_of(L, [], []).
divide_by_of(L, [X|Xs], [R|Rs]):-
    L > 0,
    R is rationalize(X/L),
    divide_by_of(L, Xs, Rs).

% Predicate to create the list of floats.
uniform_list_of_floats(N, R):-
    findall(X, between(0, N, X), X),
    divide_by_of(N, X, R).

zip([], [], []).
zip([], [_|_], []).
zip([_|_], [], []).
% Recursive case: Zip the head elements of both lists and recursively zip the tails.
zip([X|Xs], [Y|Ys], [[X, Y]|Zipped]) :-
    zip(Xs, Ys, Zipped).

% Assume 2 dimensions
weight_vectors_of_size(N, V):-
    O is N - 1,
    uniform_list_of_floats(O, Floats),
    reverse(Floats, Revs),
    zip(Floats, Revs, V).
