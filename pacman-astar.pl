:- dynamic wall/2.
:- dynamic candy/2.
:- dynamic fruit/2.
:- dynamic player_a_pos/2.
:- dynamic player_b_pos/2.
:- dynamic fruit_alive/1.

:- dynamic player_a_score/1.
:- dynamic player_b_score/1.

:- dynamic next_node_id/1.
:- dynamic node/4.

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
player_a_pos(2, 10).
player_b_pos(10, 2).

% Fruit position
fruit(6, 6).
fruit_alive(1).

type(X, Y, wall) :- wall(X, Y), !.
type(X, Y, fruit) :- fruit(X, Y), !.
type(X, Y, candy) :- candy(X, Y), !.
type(X, Y, player_a) :- player_a_pos(X, Y), !.
type(X, Y, player_b) :- player_b_pos(X, Y), !.
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

write_cell(wall) :- write('\033[1;40m   \033[0m').
write_cell(player_a) :- write('\033[1;32m A \033[0m').
write_cell(player_b) :- write('\033[1;31m B \033[0m').
write_cell(fruit) :- write('\033[1;33m F \033[0m').
write_cell(candy) :- write('\033[1;33m . \033[0m').
write_cell(free) :- write('   ').

% Node generation
generate_map_nodes :-
  height(H),
  generate_map_nodes_row(H).

generate_map_nodes_row(0) :- !.
generate_map_nodes_row(H) :-
  width(W),
  generate_node(W, H),
  H1 is H - 1,
  generate_map_nodes_row(H1).

generate_node(0, _) :- !.
generate_node(W, H) :-
  assert(node(W, H, 0, 0)),
  W1 is W - 1,
  generate_node(W1, H).
  

%  ================== (FIN) FONCTIONS DE LA MAP ==================

%  ================== FONCTIONS DU DÉROULEMENT DU PROGRAMME ==================
round(1).
player_a_score(0).
player_b_score(0).

% Traduit les entrées clavier vers des valeurs lisibles
movement_key_translation(119, up).
movement_key_translation(97, left).
movement_key_translation(115, down).
movement_key_translation(100, right).
movement_key_translation(113, quit).
movement_key_translation(_, invalid).

menu_key_translation(49, 1).
menu_key_translation(50, 2).
menu_key_translation(113, quit).
menu_key_translation(_, invalid).

% Traduit les directions vers leur index
move_translate(up, 0).
move_translate(left, 1).
move_translate(down, 2).
move_translate(right, 3).

% ===== START ET QUIT =====
% On commence le programme ici
start_program :-
    writeln('Choisissez l\'une des options suivantes :'),
    writeln('1. Commencer une partie contre l\'IA'),
    writeln('2. Voir une partie d\'IA contre IA'),
    writeln('Q. Quitter'),
    handle_menu_input.

% Permet de gérer les choix du menu
handle_choice(1) :-
    nl,
    writeln('Début de la partie contre l\'IA'),
    start_human_vs_ai.
handle_choice(2) :-
    start_ai_vs_ai.
handle_choice(quit) :- 
    quit_program.
handle_choice(_) :-
    handle_menu_input,
    start_program.

% Quitte le programme
quit_program :-
    writeln('Au revoir!'),
    halt.

% Vérifie si l'utilisateur veut quitter
try_quit(quit) :-
    quit_program.
try_quit(_).
% ===== (FIN) START ET QUIT =====

% ===== GESTION DES ENTRÉES UTILISATEURS =====
% Permet de gérer les entrées de l'utilisateur sur le menu
handle_menu_input :-
    get_single_char(Input),
    menu_key_translation(Input, Choice),
    handle_choice(Choice).

% Permet de gérer les entrées de l'utilisateur pour les mouvements
handle_human_movement_input(PlayerAction) :-
    get_single_char(Input),
    movement_key_translation(Input, PlayerAction).
% ===== (FIN) GESTION DES ENTRÉES UTILISATEURS =====

% ===== GESTION DES MOVEMENTS ET DU JEU =====
% La partie de Humain contre IA commence ici
start_human_vs_ai :-
    start_round(1).

% La partie d'IA contre IA commence ici
start_ai_vs_ai :-
    % TODO
    halt.

% Commence une nouvelle manche
start_round(R) :- 
    %TODO : Vérifier si un joueur a gagné
    nl,
    show_map,
    player_a_score(ScoreA), player_b_score(ScoreB),
    write('Round '), write(R), write(' | '), write('Score A: '), write(ScoreA), write(' | '), write('Score B: '), write(ScoreB), nl,
    writeln('Vous êtes le joueur B'),
    writeln('Choisissez une direction (w, a, s, d) ou q pour quitter'),

    % On gère les mouvements du joueur
    human_turn,

    % On gère les mouvements de l'IA
    calculate_ai_move(player_a, AIMove),
    writeln('L\'IA a joué : '), writeln(AIMove),

    R1 is R + 1,
    start_round(R1).

human_turn :-
    % On gère les entrées de l'utilisateur et les mouvements du joueur
    handle_human_movement_input(PlayerAction),
    try_quit(PlayerAction), % On vérifie si l'utilisateur veut quitter, si oui, on quitte le programme
    player_b_pos(X, Y),
    validate_player_move(X, Y, PlayerAction, player_b).

% Valide et effectue le mouvement du joueur
validate_player_move(X, Y, up, Player) :-
    Y1 is Y + 1,
    check_for_no_collision(X, Y1),
    move_player_to(Player, X, Y1).
validate_player_move(X, Y, left, Player) :-
    X1 is X - 1,
    check_for_no_collision(X1, Y),
    move_player_to(Player, X1, Y).
validate_player_move(X, Y, down, Player) :-
    Y1 is Y - 1,
    check_for_no_collision(X, Y1),
    move_player_to(Player, X, Y1).
validate_player_move(X, Y, right, Player) :-
    X1 is X + 1,
    check_for_no_collision(X1, Y),
    move_player_to(Player, X1, Y).
validate_player_move(_, _, _, player_b) :-
    human_turn.

check_for_no_collision(X, Y) :-
    not(type(X, Y, wall)),
    not(type(X, Y, player_a)),
    not(type(X, Y, player_b)).

% Déplace le joueur à la nouvelle position
move_player_to(player_a, X, Y) :-
    retract(player_a_pos(_, _)),
    assert(player_a_pos(X, Y)),
    check_for_candy(player_a, X, Y),
    check_for_fruit(player_a, X, Y).
move_player_to(player_b, X, Y) :-
    retract(player_b_pos(_, _)),
    assert(player_b_pos(X, Y)),
    check_for_candy(player_b, X, Y),
    check_for_fruit(player_b, X, Y).

% Vérifie si le joueur est sur un bonbon
check_for_candy(player_a, X, Y) :-
    type(X, Y, candy),
    retract(candy(X, Y)),
    player_a_score(Score),
    NewScore is Score + 1,
    retract(player_a_score(_)),
    assert(player_a_score(NewScore)).
check_for_candy(player_b, X, Y) :-
    type(X, Y, candy),
    retract(candy(X, Y)),
    player_b_score(Score),
    NewScore is Score + 1,
    retract(player_b_score(_)),
    assert(player_b_score(NewScore)).
check_for_candy(_, _, _).

% Vérifie si le joueur est sur un fruit
check_for_fruit(player_a, X, Y) :-
    type(X, Y, fruit),
    retract(fruit(X, Y)),
    retract(fruit_alive(_)),
    assert(fruit_alive(0)),
    player_a_score(Score),
    NewScore is Score + 10,
    retract(player_a_score(_)),
    assert(player_a_score(NewScore)).
check_for_fruit(player_b, X, Y) :-
    type(X, Y, fruit),
    retract(fruit(X, Y)),
    retract(fruit_alive(_)),
    assert(fruit_alive(0)),
    player_b_score(Score),
    NewScore is Score + 10,
    retract(player_b_score(_)),
    assert(player_b_score(NewScore)).
check_for_fruit(_, _, _).
% ===== (FIN) GESTION DES MOUVEMENTS ET DU JEU =====

% ================== FONCTIONS DU DÉROULEMENT DU PROGRAMME ==================

% ================== GESTION DE L'IA =====================

% Calcule le mouvement de l'IA
calculate_ai_move(player_a, Move) :-
    player_a_pos(X, Y),
    calculate_best_move(X, Y, Move).
calculate_ai_move(player_b, Move) :-
    player_b_pos(X, Y),
    calculate_best_move(X, Y, Move).

% Calcule le meilleur mouvement pour l'IA
calculate_best_move(X, Y, Move) :-
    fruit(FruitX, FruitY),
    % On trouve le chemin le plus optimal entre la position actuelle et la position du fruit
    CurrentNode = (X, Y, 0, 0),
    a_star([CurrentNode], [], (FruitX, FruitY), Path),
    write('Path: '), writeln(Path),
    Move = right,
    !.

a_star([CurrentNode|_], Visited, Goal, Path) :-
    CurrentNode = (X, Y, _, _),
    Goal = (X, Y),
    Path = [CurrentNode|Visited],  % le but est atteint
    !.
a_star([], _, _, _) :- !.
a_star([CurrentNode|Rest], Visited, Goal, Path) :-
    CurrentNode = (X, Y, G, _),
    findall((X1, Y1, G1, F), (
        neighbor(_, X, Y, X1, Y1),
        \+ type(X1, Y1, wall),  % pas de mur
        \+ member((X1, Y1, _, _), Visited),  % pas déjà visité
        G1 is G + 1,  % incrémenter la distance parcourue
        manhattan_distance(X1, Y1, Goal, H),
        F is G1 + H  % coût total (f = g + h)
    ), Neighbors),
    append(Neighbors, Rest, NewRest),
    predsort(compare_f, NewRest, SortedRest),
    a_star(SortedRest, [CurrentNode|Visited], Goal, Path).

% Comparer les coûts totaux F
compare_f(<, (_, _, _, F1), (_, _, _, F2)) :- F1 < F2.
compare_f(>, (_, _, _, F1), (_, _, _, F2)) :- F1 >= F2.

% Calculer la distance de Manhattan
manhattan_distance(X1, Y1, (X2, Y2), D) :-
    D is abs(X1 - X2) + abs(Y1 - Y2).

% Chercher les voisins (haut, bas, gauche, droite)
neighbor(up, X, Y, X, Y1) :- Y1 is Y + 1.
neighbor(left, X, Y, X1, Y) :- X1 is X - 1.
neighbor(down, X, Y, X, Y1) :- Y1 is Y - 1.
neighbor(right, X, Y, X1, Y) :- X1 is X + 1.


% =============== (FIN) GESTION DE L'IA ==================

% EXECUTION À PARTIR D'ICI

:- generate_walls.
:- generate_candies.
:- generate_map_nodes.
:- start_program.