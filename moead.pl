:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(pairs)).
:- use_module(library(clpfd)).
:-[lhs]
:-[tchebicheff]




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% now find a way to represent objective functions, evaluate

% evaluate objective function
evaluation_of(ObjNum, Xs, []):- n_obj(N), ObjNum is N+1.
evaluation_of(ObjNum, Xs, [Y|Ys]):-
    ObjNum1 is ObjNum + 1,
    objective(ObjNum, Xs, Y),
    evaluation_of(ObjNum1, Xs, Ys).

% now a way to evaluate initial samples
batch_evaluation_of([],[]).
batch_evaluation_of([Sub|Rest], [SubNew|RestNew]):-
    evaluation_of(1, Sub, SubNew),
    batch_evaluation_of(Rest, RestNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Need to create a method of identifing the distances between vectors and grouping them as neighbours
% these predicates are for identifing the neighbourhoods of each weight vector.
euclidean_distance_of(V1, V2, Distance) :-
    length(V1, N1),
    length(V2, N2),
    N1 =:= N2,

    % Calculate the squared differences for each element.
    squared_sum(V1, V2, SquaredSum),
    % Take the square root to get the final distance.
    Distance is sqrt(SquaredSum).

squared_sum([],[],0).
squared_sum([X|Xs], [Y|Ys], SquaredSum):-
    D is X- Y,
    SquaredDiff is D*D,
    squared_sum(Xs, Ys, RestSquaredSum),
    SquaredSum is SquaredDiff + RestSquaredSum.

% now one that identifies the neighbourhood for each vector, this is singular
t_closest_vectors(Reference, Vectors, TopT) :-
    map_distances(Reference, 0, Vectors, Distances),
    keysort(Distances, Closest),
    top_t_of(Closest, TopT).

% predicate to calculate the euclidean distance between a reference vector and a list of vectors.
map_distances(_, _, [], []).
map_distances(Reference, N, [Vector | Rest], [Distance-N | RestDistances]) :-
    euclidean_distance_of(Reference, Vector, Distance),
    N1 is N + 1,
    map_distances(Reference, N1, Rest, RestDistances).

zero_key(Key-_) :- Key =:= 0.

top_t_of(Pairs, TopT):-
    neighbourhood_size(T),
    exclude(zero_key, Pairs, NonZeroPairs),
    take(T, NonZeroPairs, SmallestPairs),
    pairs_values(SmallestPairs, TopT).

take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
    N > 0,
    N2 is N - 1,
    take(N2, TA, TB).

% This is the main predicate of this block, it defines the relation between
% the weight vectors and a nested list representing the neighbourhoods of each
% weight vector.
all_closest_neighbours_of([],[]).
all_closest_neighbours_of([Sub|Rest], [New|NewRest]):-
    weight_vectors(Vectors),
    t_closest_vectors(Sub, Vectors, New),
    print(New),
    all_closest_neighbours_of(Rest, NewRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These predicates are all about evolutionary operators.
% for selecting the indexes of the two parents, this is selection
select_two_random_of(Xs, [Y1, Y2]):-
    random_member(Y1, Xs),
    select(Y1, Xs, Xs1),
    random_member(Y2, Xs1),
    not(Y1 = Y2).

two_var_one_point_crossover_of([X1, _], [_, Y2], [X1, Y2]).

% One point crossover, for two parents, defines the relation between parents 
% and one child.
children_of(X1, X2, Child):-
    length(X1, L),
    (L =:= 2 -> two_var_one_point_crossover_of(X1, X2, Child);
    random_between(1, L, Xpoint), % randomly select a crossover point
    split_at(X1, Xpoint, Prefix, _), % split parents
    split_at(X2, Xpoint, _, Suffix), 
    append(Prefix, Suffix, Child) 
    ).

split_at(Xs, Pos, Prefix, Suffix) :-
    length(Prefix, Pos),
    append(Prefix, Suffix, Xs).


% then mutation, this is for a single solution, a single row vector
mutation_of([], []).
mutation_of([X|Xs], [M|Ms]):-
    % you should have some parameter to determine if mutation happens at all
    Prob is 0.5,
    random(0.0, 1.0, Rand),
    (Rand < Prob ->
        element_mutation_of(X, M)
    ;
        M is X
    ),
    mutation_of(Xs, Ms).

% Relation between a number and its mutant.
element_mutation_of(X, M):-
    random(0.95,1.05,R),
    M is X * R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These predicates concern the initialisation and update of the ideal point.

% update ideal point
% this is used for the initial definition of the ideal point, finds the smallest in batch
find_min_values(ObjectiveVals, MinValues) :-
    transpose(ObjectiveVals, TransposedLists), % Transpose the list of lists
    find_min_values_in_each_dimension(TransposedLists, MinValues).

find_min_values_in_each_dimension([], []).
find_min_values_in_each_dimension([Dimension|Rest], [MinValue|MinValuesRest]) :-
    min_list(Dimension, MinValue), % Find the minimum value in the current dimension
    find_min_values_in_each_dimension(Rest, MinValuesRest).


% this is for updating the ideal point, it takes a single value and compares it to
% another, defining the relation between the two lists and another list with the smallest
% elements from both
updated_ideal_point_of([], [], []).
updated_ideal_point_of([X|Xs], [Y|Ys], [Z|Zs]):-
    Z is min(X, Y),
    updated_ideal_point_of(Xs, Ys, Zs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These predicates are used to define relations that allow the updating of neighbouring 
% solutions, step 2.4.

retrieve_items_by_indices(Indices, SourceList, ResultList) :-
    retrieve_items_by_indices(Indices, SourceList, [], ResultList).

% base case: When the list of indices is empty, the result list is also empty.
retrieve_items_by_indices([], _, ResultList, ResultList).

% recursive case: Retrieve items based on the current index and continue with the rest of the indices.
retrieve_items_by_indices([Index|RestIndices], SourceList, PartialResult, ResultList) :-
    nth0(Index, SourceList, Item), % Retrieve the item at the specified index
    append(PartialResult, [Item], UpdatedPartialResult), % Add the retrieved item to the result
    retrieve_items_by_indices(RestIndices, SourceList, UpdatedPartialResult, ResultList).

% This is used for comparing tchebicheff values
% it duplicates the objective values into a nested list of identical 
% row vectors. This means we can reuse an already defined predicate
nest_list(1, Xs, [Xs]).
nest_list(N, Xs, [Xs|NestedRest]) :-
    N > 1,
    N1 is N - 1,
    nest_list(N1, Xs, NestedRest).

% this predicate compares two lists, used in step 2.4 to compare tchebicheff vals
% if X is lower than Y, Z is 1, symbolising an improvement for that subproblem
the_lower_of([],[],[]).
the_lower_of([X|Xs], [Y|Ys], [Z|Zs]):-
    ( X =< Y ->
        Z is 1
    ;
        Z is 0
    ),
    the_lower_of(Xs, Ys, Zs).

% replace element, used in updating the population
replace_element(List, Index, NewElement, UpdatedList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, UpdatedList, NewElement, Rest).

% base case: when we have processed all subproblems, stop
update_population(Init, Pop, Betters, _, _, _, Init, Pop, Index):-
    length(Betters, Length),
    Index =:= Length.

% recursive case: Process the current element based on Betters.
update_population(Init, Pop, Betters, MutantChild, ChildEval, Neighbourhood, UpdatedInit, UpdatedPop, Index) :-
    length(Betters, L),
    Index < L,
    nth0(Index, Betters, 1), % Check if the current element in Betters is 1
    nth0(Index, Neighbourhood, NeighbourIdx), % Get the corresponding index of the neighbor
    replace_element(Init, NeighbourIdx, MutantChild, NUpdatedInit), % Replace the item in the population, using the new Idx

    replace_element(Pop, NeighbourIdx, ChildEval, NUpdatedPop),
    NextIndex is Index + 1,
    update_population(NUpdatedInit, NUpdatedPop, Betters, MutantChild, ChildEval, Neighbourhood, UpdatedInit, UpdatedPop, NextIndex).

update_population(Init, Pop, Betters, MutantChild, ChildEval, Neighbourhood, UpdatedInit, UpdatedPop, Index) :-
    length(Betters, L),
    Index < L,
    nth0(Index, Betters, 0), % Check if the current element in Betters is 0
    NextIndex is Index + 1,
    update_population(Init, Pop, Betters, MutantChild, ChildEval, Neighbourhood, UpdatedInit, UpdatedPop, NextIndex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Defines the relation of domination, strict domination
dominates([],[]).
dominates([X|Xs], [Y|Ys]):-
    X < Y,
    dominates(Xs, Ys).

% for step 2.5, update the external population.
updated_EP_of([], ChildEval, [ChildEval]).
updated_EP_of(EP, ChildEval, FinalEP) :-
    % Remove from EP all vectors dominated by ChildEval.
    exclude(dominates(ChildEval), EP, FilteredEP), % this works
    
    % (\+ (member(Y, EP), dominates(Y, ChildEval)) ->
    (\+ (member(Y, EP), dominates(Y, ChildEval), \+(member(ChildEval, EP))) ->
        append([ChildEval], FilteredEP, FinalEP)
    ;
        FilteredEP = FinalEP
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These predicates define the main flow of the algorithm.
% This is the initialisation of the algorithm.
moead(Gens, NewEP, FinalSol, FinalFE):-
    % checking that the number of weight vectors is the same as the population
    population_size(PopSize),
    weight_vectors(WVectors),
    length(WVectors, L),
    L is PopSize,

    % Step 1.2
    % Get the neighbourhoods
    all_closest_neighbours_of(WVectors, Neighbourhoods),
    % Step 1.3
    initial_samples(Init),
    % find the initial samples
    batch_evaluation_of(Init, YInit),
    % Step 1.4
    % get the inital ideal point, 
    find_min_values(YInit, Ideal),
    moead_iterate_n_times(Ideal, Neighbourhoods, Init, YInit, PopSize, Gens, [], NewEP, FinalSol, FinalFE).

% iterate generations
moead_iterate_n_times(Ideal, Neighbourhoods, Init, YInit, PopSize, 0, EPAcc, FinEP, FinSol, FinFE) :-
    FinEP = EPAcc,
    FinSol = Init, % update with the final population of solutions
    FinFE = YInit. % update with the final fitness values
moead_iterate_n_times(Ideal, Neighbourhoods, Init, YInit, PopSize, Gens, EPAcc, FinEP, FinSol, FinFE) :-
    nl,
    print(generation),
    print(Gens),nl,
    Gens > 0,
    moead_iterate(Ideal, Neighbourhoods, Init, YInit, PopSize, EPAcc, NewEP, FinalSol, FinalFE, NewIdeal),
    Gens1 is Gens - 1,
    moead_iterate_n_times(NewIdeal, Neighbourhoods, FinalSol, FinalFE, PopSize, Gens1, NewEP, FinEP, FinSol, FinFE).

% this predicate is to implement the behaviour that we run "step 2" for all weight vectors.
moead_iterate(Ideal, Neighbourhood, Init, YInit, 0, EP, EP, Init, YInit, Ideal). % basecase where EP is the same as NewEP
moead_iterate(Ideal, Neighbourhoods, Init, YInit, N, EP, UpdatedEP, UpdatedSols, UpdatedFE, UpdatedIdeal):-
    N > 0,
    % get the neighbourhood
    nth1(N, Neighbourhoods, Neighbhood),

    % create a new solution
    select_two_random_of(Neighbhood, [Parent1, Parent2]),

    % parents are indexes
    nth0(Parent1, Init, ParentSol1),
    nth0(Parent2, Init, ParentSol2),

    % evolutionary operators
    children_of(ParentSol1, ParentSol2, Child),
    mutation_of(Child, MutantChild),

    evaluation_of(1, MutantChild, ChildEval),

    % 2.3 update ideal point. This needs to be changed
    updated_ideal_point_of(ChildEval, Ideal, NewIdeal),

    % 2.4 
    % Update neighbouring solutions
    % get the solutions corresponding to the indicies 
    retrieve_items_by_indices(Neighbhood, YInit, YNeighs),
    % and get the weights of each of these
    weight_vectors(W),
    retrieve_items_by_indices(Neighbhood, W, NeighbourWeights),
    % get the tcheb value of YInits, the neighbours
    batch_tcheb_of(YNeighs, NeighbourWeights, NewIdeal, YScores),

    % nest it so we can reuse the predicate batch_tcheb_of
    length(YScores, L),
    nest_list(L, ChildEval, NestedChildEval),
    % Get the scores of the mutated child relative to the weights of the neighbours.
    batch_tcheb_of(NestedChildEval, NeighbourWeights, Ideal, ChildScores),

    % now compare, pairwise Yscores and Childscores
    the_lower_of(ChildScores, YScores, Betters),

    update_population(Init, YInit, Betters, MutantChild, ChildEval, Neighbhood, UpdatedInit, UpdatedYInit, 0),

    updated_EP_of(EP, ChildEval, NewEP),

    N1 is N-1,
    moead_iterate(NewIdeal, Neighbourhoods, UpdatedInit, UpdatedYInit, N1, NewEP, UpdatedEP, UpdatedSols, UpdatedFE, UpdatedIdeal).