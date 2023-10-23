:- [moead].


objective(1, [X1,X2], O):-
    O is 100 * (X1**2 + X2**2).

objective(2, [X1,X2], O):-
    O is (X1-1)**2 + X2**2.

varaiable_ranges([[-2.0, 2.0], [-2.0, 2.0]]).

n_obj(2).
% Population Size:
population_size(15).

weight_vectors([[0, 1], [1r14, 13r14], [1r7, 6r7], [3r14, 11r14], [2r7, 5r7], [5r14, 9r14], [3r7, 4r7], [1r2, 1r2], [4r7, 3r7], [9r14, 5r14], [5r7, 2r7], [11r14, 3r14], [6r7, 1r7], [13r14, 1r14], [1, 0]]).
% T, neighbourhood size
neighbourhood_size(2).

write_variables_to_file(FileName) :-
    tell(FileName),  % Open the file for writing

    % Write unified variables to the file
    moead(175, EP, X, Y),
    write(EP), nl,
    told. 

