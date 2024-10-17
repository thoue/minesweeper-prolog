:- dynamic wall/2.
:- dynamic candy/2.
:- dynamic fruit/2.
:- dynamic player_a_pos/2.
:- dynamic player_b_pos/2.
:- dynamic fruit_eaten/1.
:- dynamic fruit_value/1.
:- dynamic fruit_round/1.
:- dynamic total_points/1.
:- dynamic round/1.

:- dynamic player_a_score/1.
:- dynamic player_b_score/1.

:- dynamic next_node_id/1.
:- dynamic node/6. % X, Y, G, F, ParentX, ParentY

:- dynamic player_a_cluster/1.
:- dynamic player_a_target_candy/2.

:- dynamic player_b_cluster/1.
:- dynamic player_b_target_candy/2.


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
wall(5, 5).
wall(5, 6).
wall(5, 7).
wall(5, 8).
wall(5, 9).

wall(5, 13).
wall(5, 14).
wall(5, 15).
wall(5, 16).
wall(5, 17).

wall(5, 17).
wall(6, 17).
wall(7, 17).
wall(8, 17).
wall(9, 17).

wall(13, 17).
wall(14, 17).
wall(15, 17).
wall(16, 17).
wall(17, 17).


wall(17, 9).
wall(17, 8).
wall(17, 7).
wall(17, 6).
wall(17, 5).

wall(17, 17).
wall(17, 16).
wall(17, 15).
wall(17, 14).
wall(17, 13).

wall(5, 5).
wall(6, 5).
wall(7, 5).
wall(8, 5).
wall(9, 5).

wall(13, 5).
wall(14, 5).
wall(15, 5).
wall(16, 5).
wall(17, 5).

wall(9, 13).
wall(10, 13).
wall(11, 13).
wall(12, 13).
wall(13, 13).

wall(9, 9).
wall(10, 9).
wall(11, 9).
wall(12, 9).
wall(13, 9).


% Map dimensions
width(21).
height(21).

% Starting positions
player_a_pos(2, 20).
player_b_pos(20, 2).

% Fruit position
fruit(11, 11).
fruit_eaten(0).
fruit_value(10).
total_points(10).
fruit_round(2).

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
    total_points(Total),
    NewTotal is Total + 1,
    retract(total_points(_)),
    assert(total_points(NewTotal)),
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

write_cell(wall) :- write('\033[1;40m # \033[0m').
write_cell(player_a) :- write('\033[1;42m A \033[0m').
write_cell(player_b) :- write('\033[1;41m B \033[0m').
write_cell(fruit) :- write('\033[1;44m O \033[0m').
write_cell(candy) :- write('\033[1;45m . \033[0m').
write_cell(free) :- write('\033[1;40m   \033[0m').

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
movement_translation(up, 0).
movement_translation(left, 1).
movement_translation(down, 2).
movement_translation(right, 3).

player_a_cluster([]).
player_a_target_candy(-1, -1).

player_b_cluster([]).
player_b_target_candy(-1, -1).

% ===== START ET QUIT =====
% On commence le programme ici
start_program :-
    nl,
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
    start_human_vs_ai_round(1).

% La partie d'IA contre IA commence ici
start_ai_vs_ai :-
    start_ai_vs_ai_round(1).

% Vérifie si un joueur a gagné
check_winner(player_a, ScoreA, HalfTotalPoints) :-
    ScoreA > HalfTotalPoints,
    writeln('Le joueur A a gagné!'),
    quit_program.
check_winner(player_b, ScoreB, HalfTotalPoints) :-
    ScoreB > HalfTotalPoints,
    writeln('Le joueur B a gagné!'),
    quit_program.
check_winner(_, _, _).

% Commence une nouvelle manche
start_human_vs_ai_round(R) :- 
    player_a_score(ScoreAB), player_b_score(ScoreBB),
    nl,
    show_map,
    write('Round '), write(R), write(' | '), write('Score A: '), write(ScoreAB), write(' | '), write('Score B: '), write(ScoreBB), nl,
    writeln('Vous êtes le joueur B'),
    writeln('Choisissez une direction (w, a, s, d) ou q pour quitter'),

    % On gère les mouvements du joueur
    human_turn,

    % On gère les mouvements de l'IA
    ai_turn(player_a),

    % On gère les mouvements du fruit
    fruit_turn,

    nl,

    player_a_score(ScoreA), player_b_score(ScoreB),
    total_points(TotalPoints),
    HalfTotalPoints is TotalPoints / 2,
    check_winner(player_a, ScoreA, HalfTotalPoints),
    check_winner(player_b, ScoreB, HalfTotalPoints),

    R1 is R + 1,
    retract(round(_)),
    assert(round(R1)),
    start_human_vs_ai_round(R1).

start_ai_vs_ai_round(R) :-
    player_a_score(ScoreAB), player_b_score(ScoreBB),

    nl,
    show_map,

    write('Round '), write(R), write(' | '), write('Score A: '), write(ScoreAB), write(' | '), write('Score B: '), write(ScoreBB), nl,
    writeln('C\'est au tour du joueur \033[1;31m B \033[0m'),
    sleep(0.5),
    ai_turn(player_b),

    nl,
    show_map,
    write('Round '), write(R), write(' | '), write('Score A: '), write(ScoreAB), write(' | '), write('Score B: '), write(ScoreBB), nl,
    writeln('C\'est au tour du joueur \033[1;32m A \033[0m'),
    ai_turn(player_a),
    sleep(0.5),

    % On gère les mouvements du fruit
    fruit_turn,

    % Si l'utilisateur appuie sur q, on quitte le programme
    get_non_blocking_char(Char),
    movement_key_translation(Char, PlayerAction),
    try_quit(PlayerAction),

    player_a_score(ScoreA), player_b_score(ScoreB),
    total_points(TotalPoints),
    HalfTotalPoints is TotalPoints / 2,
    check_winner(player_a, ScoreA, HalfTotalPoints),
    check_winner(player_b, ScoreB, HalfTotalPoints),

    R1 is R + 1,
    retract(round(_)),
    assert(round(R1)),
    start_ai_vs_ai_round(R1).

get_non_blocking_char(Char) :-
    set_stream(user_input, timeout(0.1)),
    catch(get_single_char(Char), _, Char = -1),
    set_stream(user_input, timeout(infinite)).

human_turn :-
    % On gère les entrées de l'utilisateur et les mouvements du joueur
    handle_human_movement_input(PlayerAction),
    try_quit(PlayerAction), % On vérifie si l'utilisateur veut quitter, si oui, on quitte le programme
    player_b_pos(X, Y),
    validate_player_move(X, Y, PlayerAction, player_b).

ai_turn(Player) :-
    get_player_position(Player, X, Y),
    calculate_ai_move(Player, AIMove),
    validate_player_move(X, Y, AIMove, Player).

fruit_turn :-
    fruit_eaten(0),
    fruit_round(FruitRound),
    round(Round),
    IsPlaying is mod(Round, FruitRound),
    random(0, 4, R),
    fruit(X, Y),
    movement_translation(Move, R),
    move_fruit(IsPlaying, X, Y, Move).
fruit_turn :- 
    fruit_eaten(1).

move_fruit(0, X, Y, Move) :-
    validate_player_move(X, Y, Move, fruit).
move_fruit(_, _, _, _).

get_player_position(player_a, X, Y) :- player_a_pos(X, Y).
get_player_position(player_b, X, Y) :- player_b_pos(X, Y).

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
validate_player_move(_, _, _, fruit) :-
    fruit_turn.

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
move_player_to(fruit, X, Y) :-
    retract(fruit(_,_)),
    assert(fruit(X, Y)).

% Vérifie si le joueur est sur un bonbon
check_for_candy(player_a, X, Y) :-
    type(X, Y, candy),
    retract(candy(X, Y)),
    player_a_score(Score),
    NewScore is Score + 1,
    retract(player_a_score(_)),
    assert(player_a_score(NewScore)),

    player_a_target_candy(TargetX, TargetY),
    
    (TargetX == X, TargetY == Y -> 
        retract(player_a_target_candy(_, _)),
        assert(player_a_target_candy(-1, -1))
    ; true),
    update_every_cluster(X, Y).
    

check_for_candy(player_b, X, Y) :-
    type(X, Y, candy),
    retract(candy(X, Y)),
    player_b_score(Score),
    NewScore is Score + 1,
    retract(player_b_score(_)),
    assert(player_b_score(NewScore)),

    player_b_target_candy(TargetX, TargetY),

    (TargetX == X, TargetY == Y -> 
        retract(player_b_target_candy(_, _)),
        assert(player_b_target_candy(-1, -1))
    ; true),
    update_every_cluster(X, Y).
check_for_candy(_, _, _).

update_every_cluster(X, Y) :-
    player_a_cluster(ClusterA),
    subtract(ClusterA, [(X, Y)], NewClusterA),
    update_player_cluster(player_a, NewClusterA),
    player_b_cluster(ClusterB),
    subtract(ClusterB, [(X, Y)], NewClusterB),
    update_player_cluster(player_b, NewClusterB).

% Vérifie si le joueur est sur un fruit
check_for_fruit(player_a, X, Y) :-
    type(X, Y, fruit),
    retract(fruit(X, Y)),
    retract(fruit_eaten(_)),
    assert(fruit_eaten(1)),
    player_a_score(Score),
    fruit_value(Value),
    NewScore is Score + Value,
    retract(player_a_score(_)),
    assert(player_a_score(NewScore)).
check_for_fruit(player_b, X, Y) :-
    type(X, Y, fruit),
    retract(fruit(X, Y)),
    retract(fruit_eaten(_)),
    assert(fruit_eaten(1)),
    player_b_score(Score),
    fruit_value(Value),
    NewScore is Score + Value,
    retract(player_b_score(_)),
    assert(player_b_score(NewScore)).
check_for_fruit(_, _, _).
% ===== (FIN) GESTION DES MOUVEMENTS ET DU JEU =====

% ================== GESTION DE L'IA =====================

% Calcule le mouvement de l'IA
calculate_ai_move(player_a, Move) :-
    player_a_pos(X, Y),
    calculate_best_move(X, Y, Move, player_a).
calculate_ai_move(player_b, Move) :-
    player_b_pos(X, Y),
    calculate_best_move(X, Y, Move, player_b).

% Calcule le meilleur mouvement pour l'IA
calculate_best_move(X, Y, Move, _) :-
    fruit_eaten(0),

    YUp is Y + 1,
    calculate_fruit_a_star(X, YUp, UpValue),
    XRight is X + 1,
    calculate_fruit_a_star(XRight, Y, RightValue),

    YDown is Y - 1,
    calculate_fruit_a_star(X, YDown, DownValue),

    XLeft is X - 1,
    calculate_fruit_a_star(XLeft, Y, LeftValue),

    HighestValue is max(UpValue, max(RightValue, max(DownValue, LeftValue))),
    best_move(UpValue, RightValue, DownValue, LeftValue, HighestValue, Move),
    !.
calculate_best_move(X, Y, Move, Player) :-
    % On regarde si on a déjà trouvé un cluster de bonbons est vide
    get_player_cluster(Player, Cluster),


    (Cluster == [] -> 
        % Trouver un clusteur de bonbons
        findall((X1, Y1), candy(X1, Y1), CandyList),
        find_candy_clusters(CandyList, Clusters),
        % On trouve le plus gros cluster
        find_biggest_cluster(Clusters, BiggestCluster),
        update_player_cluster(Player, BiggestCluster)
    ; BiggestCluster = Cluster),

    %write('Cluster: '), write(BiggestCluster), nl,

    get_player_target_candy(Player, CandyX, CandyY),
    get_other_player_position(Player, OtherX, OtherY),

    %write('Checking for target candy: '), write(CandyX), write(' '), writeln(CandyY),

    % On regarde si le bonbon cible est vide
    (CandyX == -1, CandyY == -1 ->
        generate_new_target_candy(Player, BiggestCluster)
    ; true),

    % On regarde si l'autre joueur est sur le bonbon cible
    (OtherX == CandyX, OtherY == CandyY ->
        generate_new_target_candy(Player, BiggestCluster)
    ; true),

    %get_player_target_candy(Player, TargetX, TargetY),
    %write('New target candy: '), write(TargetX), write(' '), writeln(TargetY),

    YUp is Y + 1,
    calculate_candy_a_star(X, YUp, UpValue, Player),
    XRight is X + 1,
    calculate_candy_a_star(XRight, Y, RightValue, Player),

    YDown is Y - 1,
    calculate_candy_a_star(X, YDown, DownValue, Player),

    XLeft is X - 1,
    calculate_candy_a_star(XLeft, Y, LeftValue, Player),

    HighestValue is max(UpValue, max(RightValue, max(DownValue, LeftValue))),
    best_move(UpValue, RightValue, DownValue, LeftValue, HighestValue, Move),
    !.

% Génère un nouveau bonbon cible pour l'IA
generate_new_target_candy(Player, Cluster) :-
    random_member((TargetX, TargetY), Cluster),
    update_player_target_candy(Player, TargetX, TargetY).

% Retourne le cluster selon le joueur
get_player_cluster(player_a, Cluster) :- player_a_cluster(Cluster).
get_player_cluster(player_b, Cluster) :- player_b_cluster(Cluster).

% Met à jour le cluster du joueur
update_player_cluster(player_a, Cluster) :- retract(player_a_cluster(_)), assert(player_a_cluster(Cluster)).
update_player_cluster(player_b, Cluster) :- retract(player_b_cluster(_)), assert(player_b_cluster(Cluster)).

% Retourne le bonbon cible du joueur
get_player_target_candy(player_a, X, Y) :- player_a_target_candy(X, Y).
get_player_target_candy(player_b, X, Y) :- player_b_target_candy(X, Y).

% Met à jour le bonbon cible du joueur	
update_player_target_candy(player_a, X, Y) :- retract(player_a_target_candy(_, _)), assert(player_a_target_candy(X, Y)).
update_player_target_candy(player_b, X, Y) :- retract(player_b_target_candy(_, _)), assert(player_b_target_candy(X, Y)).

% Retourne la position de l'autre joueur
get_other_player_position(player_a, X, Y) :- player_b_pos(X, Y).
get_other_player_position(player_b, X, Y) :- player_a_pos(X, Y).

% Trouve la valeur la plus élevée parmi les valeurs des cellules
best_move(UpValue, _, _, _, UpValue, Move) :- Move = up.
best_move(_, RightValue, _, _, RightValue, Move) :- Move = right.
best_move(_, _, DownValue, _, DownValue, Move) :- Move = down.
best_move(_, _, _, LeftValue, LeftValue, Move) :- Move = left.


% ====== PRÉDICAT POUR L'ALGORITHME A* ======
calculate_candy_a_star(X, Y, Value, Player) :-
    type(X, Y, Type),
    Type \= wall,
    Type \= player_a,
    Type \= player_b,
    path_length_to_candy(X, Y, Length, Player),
    is_candy_on_path(X, Y, CandyValue),
    Value is (1 - Length) + CandyValue,
    !.
calculate_candy_a_star(_, _, Value, _) :- Value is -1000.

% Trouve une valeur pour une cellule avec l'algorithme A*
calculate_fruit_a_star(X, Y, Value) :-
    type(X, Y, Type),
    Type \= wall,
    Type \= player_a,
    Type \= player_b,
    path_length_to_fruit(X, Y, Length),
    is_candy_on_path_fruit(X, Y, CandyValue),
    fruit_value(FruitValue),
    Value is (FruitValue - Length) + CandyValue,
    !.
calculate_fruit_a_star(_, _, Value) :- Value is -1000.

is_candy_on_path_fruit(X, Y, Value) :-
    type(X, Y, candy),
    Value is 1,
    !.
is_candy_on_path_fruit(_, _, Value) :-
    Value is 0,
    !.

is_candy_on_path(X, Y, Value) :-
    type(X, Y, candy),
    Value is 5,
    !.
is_candy_on_path(_, _, Value) :-
    Value is 0,
    !.

% Calcule la longueur du chemin pour atteindre le bonbon
path_length_to_candy(X, Y, Length, Player) :-
    get_player_target_candy(Player, CandyX, CandyY),
    CurrentNode = (X, Y, 0, 0, -1, -1),
    a_star([CurrentNode], [], (CandyX, CandyY), Visited),
    find_path(CandyX, CandyY, Visited, Path),
    length(Path, Length).

% Calcule la longueur du chemin pour atteindre le fruit
path_length_to_fruit(X, Y, Length) :-
    fruit(FruitX, FruitY),
    CurrentNode = (X, Y, 0, 0, -1, -1),
    a_star([CurrentNode], [], (FruitX, FruitY), Visited),
    find_path(FruitX, FruitY, Visited, Path),
    length(Path, Length).

% Algorithm A*
a_star([CurrentNode|_], Visited, Goal, Path) :-
    CurrentNode = (X, Y, _, _, _, _),
    Goal = (X, Y),
    Path = [CurrentNode|Visited],  % le but est atteint
    !.
a_star([], _, _, _) :- !.
a_star([CurrentNode|Rest], Visited, Goal, Path) :-
    CurrentNode = (X, Y, G, _, _, _),
    findall((X1, Y1, G1, F, ParentX, ParentY), (
        neighbor(_, X, Y, X1, Y1),
        \+ type(X1, Y1, wall), % pas de mur
        \+ type(X1, Y1, player_a), % pas de joueur a
        \+ type(X1, Y1, player_b), % pas de joueur b
        \+ member((X1, Y1, _, _), Visited),  % pas déjà visité
        G1 is G + 1,  % incrémenter la distance parcourue
        manhattan_distance(X1, Y1, Goal, H),
        F is G1 + H,  % coût total (f = g + h)
        ParentX is X,
        ParentY is Y
    ), Neighbors),
    append(Neighbors, Rest, NewRest),
    predsort(compare_f, NewRest, SortedRest),
    a_star(SortedRest, [CurrentNode|Visited], Goal, Path).

% Comparer les coûts totaux F
compare_f(<, (_, _, _, F1, _, _), (_, _, _, F2, _, _)) :- F1 < F2.
compare_f(>, (_, _, _, F1, _, _), (_, _, _, F2, _, _)) :- F1 >= F2.

% Calculer la distance de Manhattan
manhattan_distance(X1, Y1, (X2, Y2), D) :-
    D is abs(X1 - X2) + abs(Y1 - Y2).

% Chercher les voisins (haut, bas, gauche, droite)
neighbor(up, X, Y, X, Y1) :- Y1 is Y + 1.
neighbor(left, X, Y, X1, Y) :- X1 is X - 1.
neighbor(down, X, Y, X, Y1) :- Y1 is Y - 1.
neighbor(right, X, Y, X1, Y) :- X1 is X + 1.

% Trouver le chemin exact pour atteindre le fruit
find_path(-1, -1, _, Path) :-
    Path = [],
    !.
find_path(X, Y, Visited, Path) :-
    member((X, Y, _, _, ParentX, ParentY), Visited),
    find_path(ParentX, ParentY, Visited, Path1),
    Path = [(X, Y)|Path1].

% ====== (FIN)  PRÉDICAT POUR L'ALGORITHME A* ======

% ====== PRÉDICAT POUR LA GESTION DES CLUSTERS DE BONBONS ======
% Trouve le meilleur cluster de bonbons
find_candy_clusters([], FinalClusters) :- FinalClusters = [].
find_candy_clusters([CurrentCandy|Rest], FinalClusters) :-
    find_cluster_near_candy([CurrentCandy], [], Cluster),
    % On enlève les bonbons du cluster trouvé	
    subtract(Rest, Cluster, NewRest),
    find_candy_clusters(NewRest, Clusters),
    append([Cluster], Clusters, FinalClusters).

% Trouve un cluster de bonbons près d'un bonbon
find_cluster_near_candy(Open, Visited, FinalCluster) :-
    Open = [],
    FinalCluster = Visited,
    !.
find_cluster_near_candy([CurrentCandy|Rest], Visited, FinalCluster) :-
    CurrentCandy = (X, Y),
    findall((X1, Y1), (
        neighbor(_, X, Y, X1, Y1),
        type(X1, Y1, candy),
        \+ member((X1, Y1), Visited),
        \+ member((X1, Y1), Rest)
    ), Neighbors),
    append(Neighbors, Rest, NewRest),
    find_cluster_near_candy(NewRest, [CurrentCandy|Visited], FinalCluster).

% Trouve le cluster le plus gros
find_biggest_cluster([], BiggestCluster) :- BiggestCluster = [].
find_biggest_cluster([CurrentCluster|Rest], BiggestCluster) :-
    length(CurrentCluster, CurrentClusterLength),
    find_biggest_cluster(Rest, RestBiggestCluster),
    length(RestBiggestCluster, RestBiggestClusterLength),
    (CurrentClusterLength > RestBiggestClusterLength -> BiggestCluster = CurrentCluster; BiggestCluster = RestBiggestCluster).

% =============== (FIN) GESTION DE L'IA ==================

% EXECUTION À PARTIR D'ICI

:- generate_walls.
:- generate_candies.
:- start_program.