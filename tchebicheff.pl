:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(pairs)).
:- use_module(library(clpfd)).

% Predicates to calculate the tchebicheff values of row vectors.
tchebicheff_helper([], [], [], 0).
tchebicheff_helper([W|Ws], [P|Ps], [X|Xs], Score) :-
    tchebicheff_helper(Ws, Ps, Xs, SubScore),
    TempScore is W * max(0, X - P),
    Score is max(TempScore, SubScore).

% predicate defining the relation between the input and 
% output of a Tchebicheff function.
% Used to calc the Tcheb value of a single row vector.
tchebicheff_of(Weight, Ideal, Solution, Score):-
    tchebicheff_helper(Weight, Ideal, Solution, Score).

% Batch Tchebicheff computation of nested row vectors.
batch_tcheb_of([], _, _, []).
batch_tcheb_of([Sub|Rest], [Weight|WRest], Ideal, [S|Ss]):-
    tchebicheff_of(Weight, Ideal, Sub, S),
    batch_tcheb_of(Rest, WRest, Ideal, Ss).
