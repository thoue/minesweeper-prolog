:- dynamic wall/2.
:- dynamic candy/2.
:- dynamic fruit/2.
:- dynamic player_a_pos/2.
:- dynamic player_b_pos/2.

% Clear tous les faits de la map
clear_map :-
  retractall(wall(_, _)),
  retractall(candy(_, _)),
  retractall(fruit(_, _)),
  retractall(player_a_pos(_, _)),
  retractall(player_b_pos(_, _)).

% Doit être exécuté avant de commencer le jeu au cas où des faits existent déjà
:- clear_map.

%  ================== FONCTIONS DE LA MAP ==================

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

% Map dimensions
width(11).
height(11).

% Starting positions
alpha_pos(2, 10).
beta_pos(10, 2).

% Fruit position
fruit(6, 6).
fruit_alive(1).

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
%  ================== (FIN) FONCTIONS DE LA MAP ==================

%  ================== FONCTIONS DU GAMELOOP ==================
round(1).

% Translate key code to human readable key
key_translate(119, up).
key_translate(97, left).
key_translate(115, down).
key_translate(100, right).
key_translate(49, 1).
key_translate(50, 2).
key_translate(113, q).
key_translate(_, invalid).

% Translate move to index
move_translate(up, 0).
move_translate(left, 1).
move_translate(down, 2).
move_translate(right, 3).

start_program :-
    writeln('Choisissez l\'une des options suivantes :'),
    writeln('1. Commencer une partie contre l\'IA'),
    writeln('2. Voir une partie d\'IA contre IA'),
    writeln('Q. Quitter'),
    handle_menu_input.

handle_menu_input :-
    get_single_char(Input),
    key_translate(Input, Choice),
    handle_choice(Choice).

handle_choice(1) :-
    writeln('Début de la partie contre l\'IA'),
    sleep(1),
    start_player_vs_ai.
handle_choice(2) :-
    halt.
    %start_ai_vs_ai.
handle_choice(q) :- 
    writeln('Au revoir!'),
    halt.
handle_choice(_) :-
    handle_menu_input,
    start_program.

start_player_vs_ai :-
    calculate
    start_round(1).

start_round(R) :- 
    nl,
    show_map,
    write('Round '), writeln(R),
    writeln('Choisissez une direction (w, a, s, d) ou q pour quitter').



%  ================== (FIN) FONCTIONS DU GAMELOOP ==================

% EXECUTION À PARTIR D'ICI

:- generate_walls.
:- generate_candies.
:- start_program.