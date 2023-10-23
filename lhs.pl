% Predicate to run perm_of/2 n times and create a nested list
run_perm_of_n_times(N, Dims, Results) :-
    run_perm_of_n_times(N, Dims, [], Results).

% when K is 0, no more runs are needed.
run_perm_of_n_times(_, 0, Acc, Acc).

% recursive case: run perm_of and accumulate the result.
run_perm_of_n_times(N, Dims, Acc, Results) :-
    Dims > 0,
    perm_of(N, Result),
    divide_by_of(N, Result, Unires),    % Call your perm_of/2 predicate
    Dims1 is Dims - 1,
    % run_perm_of_n_times(N1, K, [Unires | Acc], Results).
    run_perm_of_n_times(N, Dims1, [Unires | Acc], Results).

perm_of(N, Samples):-
    findall(X, between(1, N, X), X),
    random_permutation(X, Samples). % now do it n times


% Scale inputs from range 0,1 to any given range.
scaled_of([], _, _, []).
scaled_of( [X|Xs], Min, Max, [Y|Ys] ):-
    % print(Y),
    % Y is (X - Min) / (Max - Min),
    Y is X*(Max - Min) + Min,

    scaled_of(Xs, Min, Max, Ys).

% 
scaled_nested_of([],[],[]).
scaled_nested_of([Sub|Rest], [[Min,Max]|RangeRest], [SubNew|RestNew]):-
    % print(Sub),
    scaled_of(Sub, Min, Max, SubNew),
    % print(SubNew),
    scaled_nested_of(Rest, RangeRest, RestNew).


% this assumes 2 dimensions as zip/3 assumes two dimensions, zip/3.
% zip/3 must be changed to accomadate samples with more than 2 dims.
initial_samples(Init):-
    n_obj(K),
    population_size(N),
    run_perm_of_n_times(N, K, UniHC), % this gives the non-scaled LHS
    varaiable_ranges(Ranges),
    print(Ranges),
    scaled_nested_of(UniHC, Ranges, [Xs,Ys]),
    zip(Xs, Ys, Init). % scaled to specified range