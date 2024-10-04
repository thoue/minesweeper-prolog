% Define a list of predefined positions
predefined_positions([
    (1, 1), % Start 1
    % Free tiles for Start 1
    (2, 1), 
    (2, 2),
    (1, 2), 
    (16, 16), % Start 2
    % Free tiles for Start 2
    (15, 15), 
    (15, 16),
    (16, 15),
    % Middle of the map
    (8, 8),
    (8, 9),
    (9, 8),
    (9, 9)
]).

% Generate cleared positions from the predefined list
generate_cleared_positions :-
    predefined_positions(Positions),
    assert_cleared_positions(Positions).

% Helper predicate to assert each position in the list
assert_cleared_positions([]).
assert_cleared_positions([(X, Y) | Rest]) :-
    assert(cleared(X, Y)),
    assert_cleared_positions(Rest).

% Define the cleared_position predicate
cleared_position(X, Y) :-
    cleared(X, Y).

generate_wall :-
    random_between(1, 16, X),
    random_between(1, 16, Y),
    ( \+ cleared_position(X, Y) ->
        assert(wall(X, Y)),
        writeln(['Wall generated at:', X, Y])
    ; 
        writeln(['Position already cleared:', X, Y]),
        fail
    ).

generate_walls(0) :- writeln('All walls generated.').  % Base case: if N = 0, stop.

generate_walls(N) :-
    writeln(['Generating wall, remaining:', N]),
    ( generate_wall ->
        N1 is N - 1,
        generate_walls(N1)
    ; 
        writeln('Retrying wall generation...'),
        generate_walls(N)
    ).

% Generate candies where there are no walls
generate_candy(RetryLimit) :-
    RetryLimit > 0,
    random_between(1, 16, X),
    random_between(1, 16, Y),
    ( \+ wall(X, Y) ->
        assert(candy(X, Y)),
        writeln(['Candy generated at:', X, Y])
    ; 
        writeln(['Position already occupied by a wall:', X, Y]),
        NewRetryLimit is RetryLimit - 1,
        generate_candy(NewRetryLimit)
    ).

generate_candies(0) :- writeln('All candies generated.').  % Base case: if N = 0, stop.

generate_candies(N) :-
    writeln(['Generating candy, remaining:', N]),
    ( generate_candy(10) ->  % Retry limit set to 10
        N1 is N - 1,
        generate_candies(N1)
    ; 
        writeln('Retrying candy generation...'),
        generate_candies(N)
    ).

%%% Helper functions %%%
available_spaces(Available) :-
    findall(_, wall(_, _), Walls),
    length(Walls, NumWalls),
    Total is 16 * 16,
    Available is Total - NumWalls.

init_map :-
    writeln('Initializing map...'),
    generate_cleared_positions,
    writeln('Cleared positions generated.'),
    generate_walls(50), % Adjust the number of walls
    writeln('Walls generated.'),
    available_spaces(Available),
    writeln(['Available spaces for candies:', Available]),
    generate_candies(Available),
    writeln('Candies generated.').

:- init_map.