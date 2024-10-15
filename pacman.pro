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
:- dynamic alpha_pos/2.
:- dynamic beta_pos/2.
:- dynamic round/1.
:- dynamic alpha_points/1.
:- dynamic beta_points/1.

:- dynamic fruit_alive/1.
:- dynamic fruit_points/1.
:- dynamic fruit_round/1.

:- dynamic tree_node/8. % ParentId, NodeId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY, if fruit not alive, FruitX and FruitY are -1
:- dynamic max_tree_depth/1.
:- dynamic next_node_id/1.
:- dynamic fruit_depth/1.

type(X, Y, wall) :- wall(X, Y), !.
type(X, Y, fruit) :- fruit(X, Y), !.
type(X, Y, candy) :- candy(X, Y), !.
type(X, Y, alpha) :- alpha_pos(X, Y), !.
type(X, Y, beta) :- beta_pos(X, Y), !.
type(_, _, free).

% Border Wall generation 
generate_top_wall :- 
  width(W),
  assert(wall(W, 1)),
  W1 is W - 1,
  generate_next_top_wall(W1).

generate_next_top_wall(0) :- !.
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

generate_next_bottom_wall(0) :- !.
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

generate_next_left_wall(1) :- !.
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

generate_next_right_wall(1) :- !.
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

generate_candy_row(0) :- !.
generate_candy_row(H) :-
  width(W),
  generate_candy(W, H),
  H1 is H - 1,
  generate_candy_row(H1).

generate_candy(W, _) :- 
  W == 0,
  !.

generate_candy(W, H) :-
  type(W, H, Type),
  random(0, 2, R),
  generate_candy(W, H, Type, R),
  W1 is W - 1,
  generate_candy(W1, H).

% If type is not free, skip
generate_candy(_, _, Type, _) :-
  Type \= free, 
  !.
% If random number is 0, skip
generate_candy(_, _, _, 0) :- !.
generate_candy(W, H, free, 1) :-
  assert(candy(W, H)),
  !.

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
write_cell(alpha) :- write(' A ').
write_cell(beta) :- write(' B ').
write_cell(fruit) :- write(' O ').
write_cell(candy) :- write(' . ').
write_cell(free) :- write('   ').

% Clear all facts
clear_map :-
  retractall(wall(_, _)),
  retractall(candy(_, _)),
  retractall(fruit(_, _)),
  retractall(alpha_pos(_, _)),
  retractall(beta_pos(_, _)).

startGame :-
  round(R),
  startRound(R).

startRound(R) :- 
  show_map,
  write('Round '),
  write(R),
  write(' starts! '),
  writeln('You are B.'),
  writeln('Press Q to quit.'),

  % Show points
  alpha_points(AP),
  beta_points(BP),
  write('Alpha: '),
  write(AP),
  write(' points, Beta: '),
  write(BP),
  writeln(' points.'),
  writeln('Press W, A, S or D to move up, left, down or right respectively.'),
  
  get_user_move,
  get_ai_move,
  get_fruit_move,

  R1 is R + 1,
  retract(round(R)),
  assert(round(R1)),
  startRound(R1).

quit(113) :- writeln('Quitting...'), halt.
quit(46) :- writeln('Quitting...'), halt.

get_user_move :-
  % read input and put it in the direction variable
  get_single_char(Input),
  handle_input(Input).

handle_input(119) :- beta_move(up), !.
handle_input(97) :- beta_move(left), !.
handle_input(115) :- beta_move(down), !.
handle_input(100) :- beta_move(right), !.
handle_input(113) :- quit(113), !.
handle_input(46) :- quit(46), !.
handle_input(_) :- get_user_move.

get_ai_move :-
  % Set root node
  retractall(tree_node(_, _, _, _, _, _, _, _)),
  alpha_pos(AlphaX, AlphaY),
  beta_pos(BetaX, BetaY),
  fruit(FruitX, FruitY),
  assert(tree_node(0, 1, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY)),

  % Set fruit next moving depth
  fruit_round(FruitRound),
  round(Round),
  FruitMovingNextMovingRound is mod(Round, FruitRound),
  FruitMovingNextMovingDepth is FruitMovingNextMovingRound * 2 + 3,
  assert(fruit_next_moving_depth(FruitMovingNextMovingDepth)),

  % Start tracking node id
  retractall(next_node_id(_)),
  assert(next_node_id(2)),
  add_children_node(alpha, 2, 1, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY),

  % Print all tree nodes
  print_all_tree_nodes,
  !.

% Define the print_all_tree_nodes/0 predicate
print_all_tree_nodes :-
  findall(tree_node(ParentId, NodeId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY), 
          tree_node(ParentId, NodeId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY), 
          TreeNodes),
  writeln('Tree nodes:'),
  writeln(TreeNodes).


add_children_node(Entity, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY) :-
  max_tree_depth(MaxTreeDepth),
  Depth < MaxTreeDepth,
  add_child_node(up, Entity, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY),
  add_child_node(right, Entity, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY),
  add_child_node(down, Entity, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY),
  add_child_node(left, Entity, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY).
add_children_node(_, _, _, _, _, _, _, _, _) :- 
  !.

add_child_node(Direction, alpha, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY) :-
  next_player_position(Direction, AlphaX, AlphaY, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= beta,
  next_node_id(NodeId),
  fruit_alive_in_tree(X1, Y1, FruitX, FruitY, FruitX1, FruitY1),
  assert(tree_node(ParentId, NodeId, X1, Y1, BetaX, BetaY, FruitX1, FruitY1)),
  retract(next_node_id(NodeId)),
  NodeId1 is NodeId + 1,
  assert(next_node_id(NodeId1)),
  Depth1 is Depth + 1,
  add_children_node(beta, Depth1, NodeId, X1, Y1, BetaX, BetaY, FruitX1, FruitY1),
  !.
add_child_node(Direction, beta, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY) :-
  next_player_position(Direction, BetaX, BetaY, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= alpha,
  next_node_id(NodeId),
  fruit_alive_in_tree(AlphaX, AlphaY, FruitX, FruitY, FruitX1, FruitY1),
  assert(tree_node(ParentId, NodeId, AlphaX, AlphaY, X1, Y1, FruitX1, FruitY1)),
  retract(next_node_id(NodeId)),
  NodeId1 is NodeId + 1,
  assert(next_node_id(NodeId1)),

  Depth1 is Depth + 1,
  add_children_node(fruit, Depth1, NodeId, AlphaX, AlphaY, X1, Y1, FruitX1, FruitY1),
  !.

add_child_node(Direction, fruit, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY) :-
  FruitX > -1,
  FruitY > -1,
  fruit_next_moving_depth(FruitMovingNextMovingDepth),
  Depth == FruitMovingNextMovingDepth,
  next_fruit_position(Direction, FruitX, FruitY, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= alpha,
  Type \= beta,
  next_node_id(NodeId),
  assert(tree_node(ParentId, NodeId, AlphaX, AlphaY, BetaX, BetaY, X1, Y1)),

  % Set next moving depth
  fruit_round(FruitRound),
  FruitMovingNextMovingDepth1 is FruitMovingNextMovingDepth + 2 * FruitRound + 1,
  retract(fruit_next_moving_depth(FruitMovingNextMovingDepth)),
  assert(fruit_next_moving_depth(FruitMovingNextMovingDepth1)),

  % Set next node id
  retract(next_node_id(NodeId)),
  NodeId1 is NodeId + 1,
  assert(next_node_id(NodeId1)),

  % Update Depth
  Depth1 is Depth + 1,
  add_children_node(alpha, Depth1, NodeId, AlphaX, AlphaY, BetaX, BetaY, X1, Y1),
  !.

add_child_node(_, fruit, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY) :-
  add_children_node(alpha, Depth, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY),
  !.

add_child_node(_, _, _, _, _, _, _, _, _, _) :- !.

fruit_alive_in_tree(PlayerX, PlayerY, FruitX, FruitY, FruitX1, FruitY1) :-
  PlayerX == FruitX,
  PlayerY == FruitY,
  FruitX1 is -1,
  FruitY1 is -1,
  !.
fruit_alive_in_tree(_, _, FruitX, FruitY, FruitX, FruitY) :- !.

% Logic to handle fruit movement

get_fruit_move :-
  fruit_alive(FruitAlive),
  fruit_round(FruitRound),
  round(Round),
  FruitMoving is mod(Round, FruitRound),
  move_fruit(FruitAlive, FruitMoving).

move_fruit(0, _) :- !.
move_fruit(1, 0) :- 
  fruit(X, Y),
  random(0, 4, Direction),
  move_fruit(Direction, X, Y).
move_fruit(1, _) :- !.

move_fruit(Direction, X, Y) :-
  next_fruit_position(Direction, X, Y, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= alpha,
  Type \= beta,
  retract(fruit(X, Y)),
  assert(fruit(X1, Y1)),
  !.

move_fruit(Direction, X, Y) :- 
  next_fruit_position(Direction, X, Y, X1, Y1),
  type(X1, Y1, Type),
  Type \= free,
  Direction1 is mod(Direction + 1, 4),
  move_fruit(Direction1, X, Y).

next_fruit_position(0, X, Y, X, Y1) :- Y1 is Y + 1.
next_fruit_position(1, X, Y, X1, Y) :- X1 is X + 1.
next_fruit_position(2, X, Y, X, Y1) :- Y1 is Y - 1.
next_fruit_position(3, X, Y, X1, Y) :- X1 is X - 1.

% Logic to hanlde movement for alpha and beta
beta_move(Direction) :-
  beta_pos(X, Y),
  next_player_position(Direction, X, Y, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= beta,
  Type \= alpha,
  eat_candy(X1, Y1),
  eat_fruit(X1, Y1),
  retract(beta_pos(X, Y)),
  assert(beta_pos(X1, Y1)),
  !.
beta_move(_) :-
  get_user_move,
  !.

alpha_move(Direction) :-
  alpha_pos(X, Y),
  next_player_position(Direction, X, Y, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= alpha,
  Type \= beta,
  eat_candy(X1, Y1),
  eat_fruit(X1, Y1),
  retract(alpha_pos(X, Y)),
  assert(alpha_pos(X1, Y1)),
  !.
alpha_move(_) :-
  get_user_move,
  !.

next_player_position(up, X, Y, X, Y1) :- Y1 is Y + 1.
next_player_position(down, X, Y, X, Y1) :- Y1 is Y - 1.
next_player_position(left, X, Y, X1, Y) :- X1 is X - 1.
next_player_position(right, X, Y, X1, Y) :- X1 is X + 1.

% Logic to handle eating of candy and fruit
eat_candy(X, Y) :-
  candy(X, Y),
  retract(candy(X, Y)),
  beta_points(P),
  P1 is P + 1,
  retract(beta_points(P)),
  assert(beta_points(P1)),
  !.
eat_candy(_, _).

eat_fruit(X, Y) :-
  fruit(X, Y),
  retract(fruit(X, Y)),
  assert(fruit(-1, -1)),
  beta_points(P),
  fruit_points(FP),
  P1 is P + FP,
  retract(beta_points(P)),
  assert(beta_points(P1)),
  retract(fruit_alive(_)),
  assert(fruit_alive(0)),
  !.
eat_fruit(_, _).


% => EXECUTION STARTS HERE <=

:- clear_map.

% Map dimensions
width(11).
height(11).

% Starting positions
alpha_pos(2, 10).
beta_pos(10, 2).

% Fruit position
fruit(6, 6).
fruit_alive(1).

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

:- generate_walls.
:- generate_candies.

% Set initial points
alpha_points(0).
beta_points(0).

% Set max tree depth for alpha-beta
max_tree_depth(5). % With 10, the ai will look 4 rounds ahead with a fruit_round of 2

% Set fruit points
fruit_points(10).
fruit_round(2).

% Start the first round
round(1).
:- startGame.