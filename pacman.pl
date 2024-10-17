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

/*    
    22 # # # # # # # # # # # # # # # # # # # # # #
    21 #   . . . . . . . . . . . . . . . . . . . #
    20 #   . . . . # . . . . . # . . . . . . . . #
    19 #   . # . . # . # # # . # . # . . # . . . #
    18 #   . # . . # . #   # . # . # . . # . . . #
    17 #   . . . . # . #   # . # . . . . . . . . #
    16 #   # # # . # . #   # . # # # # # # . . . #
    15 # . . . . . . . #   . . . . # . . . . . . #
    14 # # # # # # . # # . # # # # . # # # . # . #
    13 # . . . . . . # . . . . . . . . . . . # . #
    12 # # . # # # . # . # . . . # # # # . . # . #
    11 # # . . . . . . . . O . . . . . . . . # . #
    10 # # . # # # . # . # # # . # # # # . . # . #
    9  # # . . . . . # . . . . . # . . . . . # . #
    8  # # # # # # . # # . # # # # . # # # . # . #
    7  # . . . . . . # . . . . . . . . . # . # . #
    6  # # # # # # # # . # . # . # # # # # . # . #
    5  # # . . . . . . . # . # . . . . . . . # . #
    4  # # . # . # . . # # . # . # . # # # # # . #
    3  # # . # . # . . # . . # . # . # . . . # . #
    2  # . . . . . # # # # # # . . . . . . . .   #
    1  # # # # # # # # # # # # # # # # # # # # # #
       1 2 3 4 5 6 7 8 9 1 1 1 1 1 1 1 1 1 1 2 2 2
                         0 1 2 3 4 5 6 7 8 9 0 1 2
*/

:- dynamic wall/2.
:- dynamic candy/2.
:- dynamic fruit/2.
:- dynamic player_a_pos/2.
:- dynamic player_b_pos/2.
:- dynamic round/1.
:- dynamic player_a_points/1.
:- dynamic player_b_points/1.

:- dynamic fruit_alive/1.
:- dynamic fruit_points/1.
:- dynamic fruit_round/1.

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

show_cell(C, _) :- 
  width(W),
  C > W,
  !.
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
  player_a_points(AP),
  player_b_points(BP),
  write('Alpha: '),
  write(AP),
  write(' points, Beta: '),
  write(BP),
  writeln(' points.'),
  writeln('Press W, A, S or D to move up, left, down or right respectively.'),
  
  get_player_b_move,
  get_ai_move,
  get_fruit_move,

  R1 is R + 1,
  retract(round(R)),
  assert(round(R1)),
  startRound(R1).

quit(113) :- writeln('Quitting...'), halt.
quit(46) :- writeln('Quitting...'), halt.

get_player_b_move :-
  % read input and put it in the direction variable
  get_single_char(Input),
  handle_input(Input).

handle_input(119) :- move_player_b(0), !.
handle_input(100) :- move_player_b(1), !.
handle_input(115) :- move_player_b(2), !.
handle_input(97) :- move_player_b(3), !.
handle_input(122) :- 
  % Print tree nodes
  findall( 
    tree_node(ParentId, NodeId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY, AvailableCandies, AlphaPoints, BetaPoints),
    tree_node(ParentId, NodeId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY, AvailableCandies, AlphaPoints, BetaPoints),
    Nodes
  ),
  writeln(Nodes),
  get_player_b_move.
handle_input(113) :- quit(113), !.
handle_input(46) :- quit(46), !.
handle_input(_) :- get_player_b_move.

get_ai_move :-
  % Set root node
  retractall(tree_node(_, _, _, _, _, _, _, _, _, _, _)),
  alpha_pos(AlphaX, AlphaY),
  beta_pos(BetaX, BetaY),
  fruit(FruitX, FruitY),
  player_a_points(AlphaPoints),
  player_b_points(BetaPoints),
  findall(CandyX-CandyY, candy(CandyX, CandyY), AvailableCandies),
  assert(tree_node(0, 1, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY, AvailableCandies, AlphaPoints, BetaPoints)),

  % Set fruit next moving depth
  fruit_round(FruitRound),
  round(Round),
  FruitMovingNextMovingRound is mod(Round, FruitRound),
  FruitMovingNextMovingDepth is FruitMovingNextMovingRound * 2 + 3,
  assert(fruit_next_moving_depth(FruitMovingNextMovingDepth)),

  % Start tracking node id
  retractall(next_node_id(_)),
  assert(next_node_id(2)),
  add_children_node(alpha, 2, 1, AlphaValue, Direction, -1000, 1000),
  write('Player A Value: '),
  writeln(AlphaValue),
  write('Player A Value: '),
  writeln(Direction),

  % Move alpha in the direction
  alpha_move(Direction),
  !.

get_node_value(alpha, UpValue, RightValue, DownValue, LeftValue, Value) :-
  max_list([UpValue, RightValue, DownValue, LeftValue], Value),
  !.
get_node_value(beta, UpValue, RightValue, DownValue, LeftValue, Value) :-
  min_list([UpValue, RightValue, DownValue, LeftValue], Value),
  !.
get_node_value(fruit, UpValue, RightValue, DownValue, LeftValue, Value) :-
  max_list([UpValue, RightValue, DownValue, LeftValue], Value),
  !.

get_direction(UpValue, _, _, _, UpValue, Direction) :- Direction is 0, !.
get_direction(_, RightValue, _, _, RightValue, Direction) :- Direction is 1, !.
get_direction(_, _, DownValue, _, DownValue, Direction) :- Direction is 2, !.
get_direction(_, _, _, LeftValue, LeftValue, Direction) :- Direction is 3, !.

update_alpha_beta(alpha, Alpha, Beta, Value, NewAlpha, Beta) :-
  NewAlpha is max(Alpha, Value).

update_alpha_beta(beta, Alpha, Beta, Value, Alpha, NewBeta) :-
  NewBeta is min(Beta, Value).

add_children_node(Entity, Depth, ParentId, NodeValue, Direction, Alpha, Beta) :-

  Entity \= fruit,

  % Initialise les valeurs d'évaluation des enfants
  add_child_node(0, Entity, Depth, ParentId, UpValue, Alpha, Beta),
  update_alpha_beta(Entity, Alpha, Beta, UpValue, Alpha1, Beta1),

  (Alpha1 >= Beta1 -> NodeValue is Alpha1 ;
    add_child_node(1, Entity, Depth, ParentId, RightValue, Alpha1, Beta1),
    update_alpha_beta(Entity, Alpha1, Beta1, RightValue, Alpha2, Beta2),

    (Alpha2 >= Beta2 -> NodeValue is Alpha2 ;
      add_child_node(2, Entity, Depth, ParentId, DownValue, Alpha2, Beta2),
      update_alpha_beta(Entity, Alpha2, Beta2, DownValue, Alpha3, Beta3),

      (Alpha3 >= Beta3 -> NodeValue is Alpha3 ;
        add_child_node(3, Entity, Depth, ParentId, LeftValue, Alpha3, Beta3),
        get_node_value(Entity, UpValue, RightValue, DownValue, LeftValue, NodeValue),
        get_direction(UpValue, RightValue, DownValue, LeftValue, NodeValue, Direction)
      )
    )
  ),

  % write('Entity: '),  writeln(Entity),
  % write('Depth: '),   writeln(Depth),
  % write('Alpha 1: '),   writeln(Alpha1),
  % write('Beta 1: '),  writeln(Beta1),
  % write('Alpha 2: '), writeln(Alpha2),
  % write('Beta 2: '),  writeln(Beta2),
  % write('Alpha 3: '), writeln(Alpha3),
  % write('Beta 3: '),  writeln(Beta3),
  % write('Node value: '), writeln(NodeValue),
  % writeln(' '),
  !.
add_children_node(fruit, Depth, ParentId, NodeValue, Direction, Alpha, Beta) :-
  random(0, 4, RandomDirection),
  add_child_node(RandomDirection, fruit, Depth, ParentId, NodeValue, Alpha, Beta),
  Direction is 0,
  !.

calculate_next_step(Entity, Depth, ParentId, NodeValue, Alpha, Beta, _, _) :-
  max_tree_depth(MaxTreeDepth),
  Depth < MaxTreeDepth,
  add_children_node(Entity, Depth, ParentId, NodeValue, _, Alpha, Beta),
  !.
calculate_next_step(_, _, _, NodeValue, _, _, AlphaPoints, BetaPoints) :-
  % write('Reached max depth for node: '), writeln(ParentId),
  % write('Alpha points: '), writeln(AlphaPoints),
  % write('Beta points: '), writeln(BetaPoints),
  NodeValue is AlphaPoints - BetaPoints,
  % write('Node value: '), writeln(NodeValue),
  % writeln(' '),
  !.

% ADD ALPHA NODE
add_child_node(Direction, alpha, Depth, ParentId, NodeValue, Alpha, Beta) :-

  % Get the parent node informations
  tree_node(_, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY, AvailableCandies, AlphaPoints, BetaPoints),

  % Calculate next position
  next_player_position(Direction, AlphaX, AlphaY, X1, Y1),

  % Check if the next position is valid
  type(X1, Y1, Type),
  Type \= wall,
  BetaX \= X1,
  BetaY \= Y1,

  next_node_id(NodeId),

  % Calculate current state
  fruit_being_eaten_in_tree(X1, Y1, FruitX, FruitY, FruitX1, FruitY1, FruitValue),
  check_if_ate_candy(X1, Y1, AvailableCandies, AvailableCandies1, CandyValue),
  AlphaPoints1 is AlphaPoints + CandyValue + FruitValue,
  assert(tree_node(ParentId, NodeId, X1, Y1, BetaX, BetaY, FruitX1, FruitY1, AvailableCandies1, AlphaPoints1, BetaPoints)),

  % Update next node id
  retract(next_node_id(NodeId)),
  NodeId1 is NodeId + 1,
  assert(next_node_id(NodeId1)),

  % write('Alpha node added for direction: '), writeln(Direction),
  % write('NodeId: '), writeln(NodeId),
  % write('Parent ID: '), writeln(ParentId),
  % write('Next Cell Type: '), writeln(Type),
  % write('Movement: '), write(AlphaX), write(' '), write(AlphaY), write(' -> '), write(X1), write(' '), writeln(Y1),
  % write('Depth: '), writeln(Depth),
  % write('Candy value: '), writeln(CandyValue),
  % write('Fruit value: '), writeln(FruitValue),
  % write('Old score Alpha: '), writeln(AlphaPoints),
  % write('New score Alpha: '), writeln(AlphaPoints1),
  % write('Beta score: '), writeln(BetaPoints),
  % writeln(' '),

  % Update Depth
  Depth1 is Depth + 1,

  % Calculate next step
  calculate_next_step(beta, Depth1, NodeId, ChildValue, Alpha, Beta, AlphaPoints1, BetaPoints),

  % Calculate node value
  NodeValue is ChildValue,
  !.

% ADD BETA NODE
add_child_node(Direction, beta, Depth, ParentId, NodeValue, Alpha, Beta) :-

  % Get the parent node informations
  tree_node(_, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY, AvailableCandies, AlphaPoints, BetaPoints),

  % Calculate next position
  next_player_position(Direction, BetaX, BetaY, X1, Y1),

  % Check if the next position is valid
  type(X1, Y1, Type),
  Type \= wall,
  AlphaX \= X1,
  AlphaY \= Y1,

  next_node_id(NodeId),

  % Calculate current state
  fruit_being_eaten_in_tree(X1, Y1, FruitX, FruitY, FruitX1, FruitY1, FruitValue),
  check_if_ate_candy(X1, Y1, AvailableCandies, AvailableCandies1, CandyValue),
  BetaPoints1 is BetaPoints + CandyValue + FruitValue,
  assert(tree_node(ParentId, NodeId, AlphaX, AlphaY, X1, Y1, FruitX1, FruitY1, AvailableCandies1, AlphaPoints, BetaPoints1)),

  % Update next node id
  retract(next_node_id(NodeId)),
  NodeId1 is NodeId + 1,
  assert(next_node_id(NodeId1)),

  % write('Beta node added for direction: '), writeln(Direction),
  % write('NodeId: '), writeln(NodeId),
  % write('Parent ID: '), writeln(ParentId),
  % write('Next Cell Type: '), writeln(Type),
  % write('Movement: '), write(BetaX), write(' '), write(BetaY), write(' -> '), write(X1), write(' '), writeln(Y1),
  % write('Depth: '), writeln(Depth),
  % write('Candy value: '), writeln(CandyValue),
  % write('Fruit value: '), writeln(FruitValue),
  % write('Old score Beta: '), writeln(BetaPoints),
  % write('New score Beta: '), writeln(BetaPoints1),
  % write('Alpha score: '), writeln(AlphaPoints),
  % writeln(' '),

  % Update Depth
  Depth1 is Depth + 1,  

  % Calculate next step
  calculate_next_step(alpha, Depth1, NodeId, ChildValue, Alpha, Beta, AlphaPoints, BetaPoints1),

  % Calculate node value
  NodeValue is ChildValue,
  !.

% ADD FRUIT NODE
add_child_node(Direction, fruit, Depth, ParentId, NodeValue, Alpha, Beta) :-
  tree_node(_, ParentId, AlphaX, AlphaY, BetaX, BetaY, FruitX, FruitY, AvailableCandies, AlphaPoints, BetaPoints),
  FruitX > -1,
  FruitY > -1,
  fruit_next_moving_depth(FruitMovingNextMovingDepth),
  Depth == FruitMovingNextMovingDepth,
  next_fruit_position(Direction, FruitX, FruitY, X1, Y1),

  % Check if the next position is valid
  type(X1, Y1, Type),
  Type \= wall,
  Type \= alpha,
  Type \= beta,

  % Calculate current state
  next_node_id(NodeId),
  assert(tree_node(ParentId, NodeId, AlphaX, AlphaY, BetaX, BetaY, X1, Y1, AvailableCandies, AlphaPoints, BetaPoints)),
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

  % Calculate next step
  calculate_next_step(beta, Depth1, NodeId, ChildValue, Alpha, Beta, AlphaPoints, BetaPoints),

  NodeValue is ChildValue,
  !.

add_child_node(_, fruit, Depth, ParentId, NodeValue, Alpha, Beta) :-
  add_children_node(beta, Depth, ParentId, NodeValue, _, Alpha, Beta),
  !.

add_child_node(_, alpha, _, _, NodeValue, _, _) :-
  NodeValue is -1000,
  !.
add_child_node(_, beta, _, _, NodeValue, _, _) :- 
  NodeValue is 1000,
  !.
add_child_node(_, fruit, _, _, NodeValue, _, _) :- 
  NodeValue is -1000,
  !.

% Calculate if fruit is alive in tree
fruit_being_eaten_in_tree(PlayerX, PlayerY, FruitX, FruitY, FruitX1, FruitY1, FruitValue) :-
  PlayerX == FruitX,
  PlayerY == FruitY,
  FruitX1 is -1,
  FruitY1 is -1,
  fruit_points(FruitValue),
  !.
fruit_being_eaten_in_tree(_, _, FruitX, FruitY, FruitX, FruitY, FruitValue) :-
  FruitValue is 0,
  !.

check_if_ate_candy(X, Y, AvailableCandies, AvailableCandies1, CandyValue) :-
  member(X-Y, AvailableCandies),
  CandyValue is 1,
  delete(AvailableCandies, X-Y, AvailableCandies1),
  !.
check_if_ate_candy(_, _, AvailableCandies, AvailableCandies, CandyValue) :-
  CandyValue is 0,
  !.

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
move_player_b(Direction) :-
  beta_pos(X, Y),
  next_player_position(Direction, X, Y, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= beta,
  Type \= alpha,
  beta_eat_candy(X1, Y1),
  beta_eat_fruit(X1, Y1),
  retract(beta_pos(X, Y)),
  assert(beta_pos(X1, Y1)),
  !.
move_player_b(_) :-
  get_player_b_move,
  !.

alpha_move(Direction) :-
  alpha_pos(X, Y),
  next_player_position(Direction, X, Y, X1, Y1),
  type(X1, Y1, Type),
  Type \= wall,
  Type \= alpha,
  Type \= beta,
  alpha_eat_candy(X1, Y1),
  alpha_eat_fruit(X1, Y1),
  retract(alpha_pos(X, Y)),
  assert(alpha_pos(X1, Y1)),
  !.

next_player_position(0, X, Y, X, Y1) :- Y1 is Y + 1.
next_player_position(1, X, Y, X1, Y) :- X1 is X + 1.
next_player_position(2, X, Y, X, Y1) :- Y1 is Y - 1.
next_player_position(3, X, Y, X1, Y) :- X1 is X - 1.

% Logic to handle eating of candy and fruit
beta_eat_candy(X, Y) :-
  candy(X, Y),
  retract(candy(X, Y)),
  player_b_points(P),
  P1 is P + 1,
  retract(player_b_points(P)),
  assert(player_b_points(P1)),
  !.
beta_eat_candy(_, _).

beta_eat_fruit(X, Y) :-
  fruit(X, Y),
  retract(fruit(X, Y)),
  assert(fruit(-1, -1)),
  player_b_points(P),
  fruit_points(FP),
  P1 is P + FP,
  retract(player_b_points(P)),
  assert(player_b_points(P1)),
  retract(fruit_alive(_)),
  assert(fruit_alive(0)),
  !.
beta_eat_fruit(_, _).

alpha_eat_candy(X, Y) :-
  candy(X, Y),
  retract(candy(X, Y)),
  player_a_points(P),
  P1 is P + 1,
  retract(player_a_points(P)),
  assert(player_a_points(P1)),
  !.
alpha_eat_candy(_, _).

alpha_eat_fruit(X, Y) :-
  fruit(X, Y),
  retract(fruit(X, Y)),
  assert(fruit(-1, -1)),
  player_a_points(P),
  fruit_points(FP),
  P1 is P + FP,
  retract(player_a_points(P)),
  assert(player_a_points(P1)),
  retract(fruit_alive(_)),
  assert(fruit_alive(0)),
  !.
alpha_eat_fruit(_, _).

% => EXECUTION STARTS HERE <=

:- clear_map.

% Map dimensions
width(9).
height(9).

% Starting positions
% alpha_pos(2, 10).
% beta_pos(10, 2).
alpha_pos(2, 8).
beta_pos(8, 2).

% Fruit position
fruit(5, 5).
fruit_alive(1).

% Interior walls
wall(3, 6).
wall(4, 6).
wall(5, 6).
wall(6, 6).
wall(7, 6).
% wall(3, 8).
% wall(4, 8).
% wall(5, 8).
% wall(6, 8).
% wall(7, 8).
% wall(8, 8).
% wall(9, 8).

wall(3, 4).
wall(4, 4).
wall(5, 4).
wall(6, 4).
wall(7, 4).
% wall(8, 4).
% wall(9, 4).

:- generate_walls.
:- generate_candies.

% Set initial points
player_a_points(0).
player_b_points(0).

% Set max tree depth for alpha-beta
max_tree_depth(15).

% Set fruit points
fruit_points(10).
fruit_round(2).

% Start the first round
round(1).
:- startGame.