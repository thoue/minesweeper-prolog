forbidden_position(0, 0). %Starting position

generate_wall :-
    random_between(0, 16, X),
    random_between(0, 16, Y),
    \+ forbidden_position(X, Y),
    assert(wall(X, Y)).  % Stores the wall in the Prolog database as wall(X, Y).

generate_walls(0).  % Base case: if N = 0, stop.

generate_walls(N) :-
    generate_wall,    % Generate one wall
    N1 is N - 1,      % Decrease N by 1
    generate_walls(N1).  % Recursively generate the remaining walls.
