% Ci-dessous ce trouve un modèle de la carte hardcodée
/*    

    11 # # # # # # # # # # #
    10 #   . . . . . . . . #
    9  # . . . . . . . . . #
    8  # . . # # # # # . . #
    7  # . . . . . . . . . #
    6  # . . . . O . . . . # 
    5  # . . . . . . . . . # 
    4  # . . # # # # # . . #
    3  # . . . . . . . . . # 
    2  # . . . . . . . .   #
    1  # # # # # # # # # # #
       1 2 3 4 5 6 7 8 9 1 1 
                         0 1
*/


:- dynamic wall/2.
:- dynamic candy/2.
:- dynamic fruit/2.
:- dynamic start/2.

width(11).
height(11).

type(X, Y, wall) :- wall(X, Y), !.
type(X, Y, candy) :- candy(X, Y), !.
type(X, Y, fruit) :- fruit(X, Y), !.
type(X, Y, start) :- start(X, Y), !.
type(_, _, free).

% Starting positions
start(2, 10).
start(10, 2).

% Fruit position
fruit(6, 6).

% Interior walls
wall(3, 8).
wall(4, 8).
wall(5, 8).
wall(6, 8).
wall(7, 8).
wall(8, 8).
wall(9, 8).

wall(3, 4).
wall(4, 4).
wall(5, 4).
wall(6, 4).
wall(7, 4).
wall(8, 4).
wall(9, 4).

% Border Wall generation 
generate_top_wall :- 
  width(W),
  assert(wall(W, 1)),
  W1 is W - 1,
  generate_next_top_wall(W1).

generate_next_top_wall(0).
generate_next_top_wall(W) :-
  assert(wall(W, 1)),
  W1 is W - 1,
  generate_next_top_wall(W1).
  
generate_bottom_wall :- 
  width(W),
  height(H),
  assert(wall(W, H)),
  W1 is W - 1,
  generate_next_bottom_wall(W1).

generate_next_bottom_wall(0).
generate_next_bottom_wall(W) :-
  height(H),
  assert(wall(W, H)),
  W1 is W - 1,
  generate_next_bottom_wall(W1).

generate_left_wall :-
  height(H),
  H1 is H - 1,
  assert(wall(1, H1)),
  H2 is H1 - 1,
  generate_next_left_wall(H2).

generate_next_left_wall(1).
generate_next_left_wall(H) :-
  assert(wall(1, H)),
  H1 is H - 1,
  generate_next_left_wall(H1).

generate_right_wall :-
  width(W),
  height(H),
  H1 is H - 1,
  assert(wall(W, H1)),
  H2 is H1 - 1,
  generate_next_right_wall(H2).

generate_next_right_wall(1).
generate_next_right_wall(H) :-
  width(W),
  assert(wall(W, H)),
  H1 is H - 1,
  generate_next_right_wall(H1).

generate_walls :-
  generate_top_wall,
  generate_bottom_wall,
  generate_left_wall,
  generate_right_wall,
  !.

% Candy generation
generate_candies :-
  height(H),
  generate_candy_row(H).

generate_candy_row(0).
generate_candy_row(H) :-
  generate_candy(1, H),
  H1 is H - 1,
  generate_candy_row(H1).

generate_candy(12, _).
generate_candy(W, H) :-
  type(W, H, Type),
  random(0, 2, R),
  generate_candy(W, H, Type, R),
  W1 is W + 1,
  generate_candy(W1, H).

generate_candy(_, _, wall, _) :- !.
generate_candy(_, _, start, _) :- !.
generate_candy(_, _, _, 0) :- !.
generate_candy(W, H, free, 1) :-
  assert(candy(W, H)).

% Show map on console
show_map :-
  height(H),
  show_row(H).

show_row(0) :- !.
show_row(R) :-
  show_cell(1, R),
  nl,
  R1 is R - 1,
  show_row(R1).

show_cell(12, _).
show_cell(C, R) :-
  type(C, R, Type),
  write_cell(Type),
  C1 is C + 1,
  show_cell(C1, R).

write_cell(wall) :- write(' # ').
write_cell(candy) :- write(' . ').
write_cell(fruit) :- write(' O ').
write_cell(free) :- write('   ').
write_cell(start) :- write(' S ').
  
:- generate_walls.
:- generate_candies.
:- show_map.
    
