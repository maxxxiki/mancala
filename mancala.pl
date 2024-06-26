:- dynamic player/1.

:- dynamic player_name/1.
:- dynamic cpu_name/1.
:- dynamic winner_name/1.

set_player(Num) :-
    retractall(player(_)),  % Remove any existing player facts
    assertz(player(Num)).   % Assert the player value as 1

get_player(Num) :-
    player(Num).

set_player_name(PlayerName) :-
    retractall(player_name(_)),
    assert(player_name(PlayerName)).

get_player_name(PlayerName) :-
    player_name(PlayerName).

set_cpu_name(CPUName) :-
    retractall(cpu_name(_)),
    assert(cpu_name(CPUName)).

get_cpu_name(CPUName) :-
    cpu_name(CPUName).

set_winner_name(WinnerName) :-
    retractall(winner_name(_)),
    assert(winner_name(WinnerName)).    

get_winner_name(WinnerName) :-
    winner_name(WinnerName).



% Start function to show current options
start :-
    write('Welcome to Mancala!'),
    nl, % Added newline for better formatting
    write('Type "play." or "play_cpu." to start a game'),
    nl, % Added newline for better formatting
    write('For rules, type "rules."'),
    nl.

% Correct the above statements to use write instead of format without arguments, and ensure proper termination with periods (.) for ending statements. 

rules :-
    write('THE RULES OF MANCALA:'), nl,
    write('Objective: have the most marbles in your mancala when the game ends.'), nl,
    nl,
    write('The game begins with Player 1 selecting a pit to take all their marbles from, and proceed to place them down into the other pits and the mancalas in a counter-clockwise direction until they have no more marbles in their hand.'), nl,
    write('Player 2 then does this action as well, and the game ends when at least one side, or the board as a whole, is completely cleared of marbles.'), nl,
    nl,
    write('Some things to remember:'), nl,
    write('1. Ending your turn when landing in your Mancala will result in it being your turn again!'), nl,
    write('2. Ending your turn in an empty pit on your side will result in that marble, as well as any marbles in the opponent\'s parallel pit, to be put into your mancala!'), nl,
    write('3. Completely running out of marbles on your side will immediately end the game, and any remaining marbles will go into your opponent\'s Mancala, so be wise when making a move!'), nl,
    nl,
    write('Now you know the rules to Mancala! HAVE FUN!! :D'), nl.

 % A random name the CPU will use if only one player is playing.
cpu_names(['Max', 'Aren', 'Yousef', 'Myles', 'Adam', 'Julian', 'Shuhab', 
            'Dr. Maher Ahmed', 'Arian', 'Nausher', 'David', 'Haluka', 'Victor', 'Yathavan', 
            'Tejesh', 'Buggy >:°D', 'George Goutziomitros', 'Trevor Simon The Grandmaster', 
            'Asif', 'Allan', 'Anson', 'Derek', 'Graeme', 'Eren Jaeger', 'Gojo Satoru', 
            'Barack Obama', 'The Donald', 'Peter Griffin', 'Eduardo', 'Shivum', 'Josh', 
            'Nathanael', 'Todd', 'Peter', 'James', 'Bartholomew', 'John', 'Philip', 'Andrew', 
            'Matthew', 'Thomas', 'Judas', 'Simon', 'Alvin', 'Theodore', 'Osaka', 'Mary Jane', 
            'Gwen', 'Stacy', 'Kanye', 'Mariana', 'Oliver', 'Nancy', 'Karen', 'Faith', 'Hope', 
            'Charity', 'Helen', 'Isabelle', 'Michelle', 'Arthur', 'Dutch', 'Hosea', 'Micah', 
            'Levi', 'SSundee', 'Crainer', 'Brimstone', 'Susan', 'Molly', 'Bill', 'Mary-Beth', 
            'Abigail', 'Charles', 'Weasel Strauss', 'Sadie', 'Kai Cenat', 'CaseOh', 'Jack', 
            'Johnson', 'LENNY!', 'Sean', 'Felix', 'Mark', 'Kieran', 'Josiah', 'Tilly', 'Uncle', 
            'Bertrum', 'Tom', 'Jerry', 'Orville', 'Colm', 'Edgar', 'Albert', 'Sally', 'Nigel', 
            'Leigh', 'Pearson', 'Leviticus', 'Cornwall', 'Herbert', 'Quagmire', 'Joe', 'Billy', 
            'Slim', 'Grant', 'Vincente', 'Santa', 'Hector', 'Gustavo', 'Walter', 'Jesse', 'Harold', 
            'Annabelle', 'Dorkins', 'Bonnie', 'Lois', 'Stewie', 'Meg', 'Chris', 'Brian', 'Thomas', 
            'Millicent', 'Abraham', 'Ian', 'Anthony', 'Cooked Cookingson']).

% Reverse the given list (example: [1,2,3] -> [3,2,1])
reverse([], []).
reverse([H|T], ReversedList) :- reverse(T, ReversedT), append(ReversedT, [H], ReversedList).

% Get list of N consecutive numbers (from 0 to N-1)
consecutive(N, List) :- consecutive(0, N-1, List).
consecutive(N, Max, List) :- (
  N > Max ->
  List = []
  ; N1 is N+1,
  List = [N|List1],
  consecutive(N1, Max, List1)
).

% Sum of all elements in a list
sum_list([], 0).
sum_list([H|T], Sum) :-
  sum_list(T, Sum1),
  Sum is Sum1 + H.

% Display the board in the desired format
display_board(GameState) :-
    GameState = game_state(board(PlayerPits, PlayerStore), board(OpponentPits, OpponentStore), _),
    reverse(OpponentPits, OpponentPitsReversed),

    % Get the names of the Player and CPU
    get_player_name(PlayerName),
    get_cpu_name(CPUName),

    nl, format("   ~s~n", [CPUName]),       % Displays Player's Name
    format("   -----------------------------------------~n"),
    format("   |    |"),
    display_pits_row(OpponentPitsReversed),
    format("   | ~|~`0t~d~2+ |=============================| ~|~`0t~d~2+ |~n", [OpponentStore, PlayerStore]),
    format("   |    |"),
    display_pits_row(PlayerPits),
    format("   -----------------------------------------~n"),
    format("   ~s <----- CURRENT PLAYER~n", [PlayerName]),       % Displays Player's Name

    % Reverse the order of the names for the next cycle
    set_player_name(CPUName),
    set_cpu_name(PlayerName).

% Helper to display a row of pits
display_pits_row([P|Ps]) :-
    format(" ~|~`0t~d~2+ |", [P]),
    display_pits_row(Ps).
display_pits_row([]) :-
    format("    |~n").


% Display number of Marbles in every pit
display_pits([P|Ps]) :- pit_tabs(P, Tabs), tab(Tabs), write(P), display_pits(Ps).
display_pits([]) :- nl.

pit_tabs(P, Tabs) :- P=<9, Tabs is 2.
pit_tabs(P, Tabs) :- P>9, Tabs is 1.

% Display the number of Marbles in both Mancalas
display_mancalas(PlayerMancala, OpponentMancala) :- write(OpponentMancala), tab(18), write(PlayerMancala), nl.

% Display information about the winner.
display_winner_information(tie) :- write('Tie').
display_winner_information(Winner) :- 
  get_winner_name(WinnerName),
  format("And the winner is ~s~n", [WinnerName]).

% Functions used to handle the user input.
read_pit(Pit, CheckForZero, PlayerBoard) :-
    repeat,
        read(Pit),
        (   integer(Pit), 1 =< Pit, Pit =< 6,
            call(CheckForZero, P, PlayerBoard, MarblesNumber), MarblesNumber > 0
        ->  true, !
        ;   writeln('Please provide a valid number of non-empty field '),
            fail
        ).

boardSize(6).

% switch_player(GameState0, GameState1)
switch_player(
    game_state(PlayerBoard, OpponentBoard, player),
    game_state(OpponentBoard, PlayerBoard, bot)
).
switch_player(
    game_state(PlayerBoard, OpponentBoard, bot),
    game_state(OpponentBoard, PlayerBoard, player)
).

% Marbles_number(N, Board, MarblesNumber) - for the given Board (list of 6 Pits)
% it returns the number of Marbles in the Nth pit and stores it under MarblesNumber
marbles_number(N, board(Board, _), MarblesNumber) :- nth0(N, Board, MarblesNumber).

% pass_Marbles(N, MarblesNumber0, GameState0, GameState2)
% Distribute the Marbles from Nth pit to the consecutive pits on player and opponent
% boards (the Marbles distribution direction should be counter-clockwise).
pass_marbles(Pit, MarblesNumber0, GameState0, GameState2) :-
  boardSize(BoardSize),
  MarblesNumber0 =< (2*BoardSize-Pit),
  pass_marbles_player_side(Pit, MarblesNumber0, GameState0, MarblesNumber1, GameState1),
  pass_marbles_opponent_side(MarblesNumber1, GameState1, _, GameState2).

pass_marbles(Pit, MarblesNumber0, GameState0, GameState3) :-
  boardSize(BoardSize),
  MarblesNumber0 > (2*BoardSize-Pit),
  pass_marbles_player_side(Pit, MarblesNumber0, GameState0, MarblesNumber1, GameState1),
  pass_marbles_opponent_side(MarblesNumber1, GameState1, MarblesNumber2, GameState2),
  pass_marbles(-1, MarblesNumber2, GameState2, GameState3).

% pass_Marbles_player_side(Pit, MarblesNumber0, GameState0, MarblesNumber1, GameState1)
% Distribute the Marbles to consecutive pits and the Mancala.
% If the last Marble was placed in an empty pit - collect it and all Marbles from
% the opposite pit and place them in player's Mancala.
% If the last Marble was placed in the Mancala - there should be another turn for the player.

% The opponent board would not change in this particular case
pass_marbles_player_side(Pit, MarblesNumber0, GameState0, MarblesNumber1, GameState1) :-
    GameState0 = game_state(board(PlayerPits0, PlayerMancala0), OpponentBoard, CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerMancala1), OpponentBoard, CurrentPlayer),
    boardSize(BoardSize),
    MarblesNumber0 > (BoardSize-Pit),
    collect_marbles(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_marbles(NextPit, MarblesNumber0, PlayerPits1, PlayerPits2),
    PlayerMancala1 is PlayerMancala0+1, % Add a Marble to the Mancala
    MarblesNumber1 is MarblesNumber0-BoardSize+Pit.

% In this case it is possible that the opponent's board may change (in case of
% opponent's Marbles capturing).
% Set the number of IntermediateMarblesNumber equal to 0.
pass_marbles_player_side(Pit, MarblesNumber, GameState0, 0, GameState2) :-
    GameState0 = game_state(board(PlayerPits0, PlayerMancala0), board(OpponentPits0, OpponentMancala), CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerMancala0), board(OpponentPits0, OpponentMancala), CurrentPlayer),
    boardSize(BoardSize),
    MarblesNumber < (BoardSize-Pit),
    collect_marbles(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_marbles(NextPit, MarblesNumber, PlayerPits1, PlayerPits2),
    check_if_beatable(Pit, MarblesNumber, GameState1, GameState2).

pass_marbles_player_side(Pit, MarblesNumber, GameState0, 0, GameState1) :-
    GameState0 = game_state(board(PlayerPits0, PlayerMancala0), OpponentBoard, CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerMancala1), OpponentBoard, CurrentPlayer),
    boardSize(BoardSize),
    MarblesNumber =:= BoardSize-Pit,
    collect_marbles(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_marbles(NextPit, MarblesNumber, PlayerPits1, PlayerPits2),
    PlayerMancala1 is PlayerMancala0+1. % Add a Marble to the Mancala

% pass_Marbles_opponent_side(MarblesNumber0, GameState0, MarblesNumber1, GameState1)
% Distribute the Marbles to consecutive pits, skipping the Mancala.
% If the pass_Marbles_player_side finished in empty pit - collect the Marbles from
% "opposite" pit and place them in player's Mancala.
pass_marbles_opponent_side(0, GameState, 0, GameState) :- !.
pass_marbles_opponent_side(MarblesNumber, GameState0, 0, GameState1) :-
    GameState0 = game_state(PlayerBoard, board(OpponentPits0, OpponentMancala), CurrentPlayer),
    GameState1 = game_state(PlayerBoard, board(OpponentPits1, OpponentMancala), CurrentPlayer),
    boardSize(BoardSize),
    MarblesNumber =< BoardSize,
    distribute_marbles(0, MarblesNumber, OpponentPits0, OpponentPits1).

pass_marbles_opponent_side(MarblesNumber, GameState0, MarblesNumber1, GameState1) :-
    GameState0 = game_state(PlayerBoard, board(OpponentPits0, OpponentMancala), CurrentPlayer),
    GameState1 = game_state(PlayerBoard, board(OpponentPits1, OpponentMancala), CurrentPlayer),
    boardSize(BoardSize),
    MarblesNumber > BoardSize,
    distribute_marbles(0, MarblesNumber, OpponentPits0, OpponentPits1),
    boardSize(BoardSize),
    MarblesNumber1 is MarblesNumber-BoardSize.

% collect_Marbles(Pit, PlayerBoard0, PlayerBoard1)
% Pick up the all the Marble from the given Pit and return the new Board.
% If the Pit ID equals -1 - do not collect any Marble from the board.
collect_marbles(-1, Board, Board) :- !.
collect_marbles(0, [_|T], [0|T]) :- !.
collect_marbles(N, [H|T], [H|T1]) :- N > 0, N1 is N-1,  collect_marbles(N1, T, T1).

% distribute_Marbles(Pit, Marbles, Board0, Board1)
% Universal function for Marbles distribution (only for the pits, does not include Mancala)
% If N is the number of selected Pit, distribute only 6-N Marbles to consecutive pits.
distribute_marbles(-1, _, Board, Board) :- !. % used in case if Marbles would circle back
distribute_marbles(_, 0, Board, Board) :- !.
distribute_marbles(_, _, [], []) :- !.
distribute_marbles(N, Marbles, [H|T], [H|T1]) :- N > 0, N1 is N-1, distribute_marbles(N1, Marbles, T, T1).
distribute_marbles(0, Marbles, [H|T], [H1|T1]) :- H1 is H+1, Marbles1 is Marbles-1, distribute_marbles(0, Marbles1, T, T1).

% check_if_beatable(Pit, MarblesNumber, GameState0, GameState1)
% Check if it is possible to beat the opponent and collect opponent's Marbles from the
% opposite pit. To do this, check if the number of Marbles in the end pit is equal to 1.
check_if_beatable(Pit, MarblesNumber, GameState0, GameState1) :-
  GameState0 = game_state(board(PlayerPits0, PlayerMancala0), board(OpponentPits0, OpponentMancala), CurrentPlayer),
  GameState1 = game_state(board(PlayerPits1, PlayerMancala1), board(OpponentPits1, OpponentMancala), CurrentPlayer),
  EndPit is Pit+MarblesNumber,
  nth0(EndPit, PlayerPits0, 1), % check if the EndPit containts 1 Marbles
  boardSize(BoardSize),
  OppositePit is BoardSize - 1 - EndPit,
  nth0(OppositePit, OpponentPits0, MarblesOnOppositePit), % check if the opposite pit contains any Marbles
  MarblesOnOppositePit > 0, !,
  collect_marbles(EndPit, PlayerPits0, PlayerPits1), % collect the one Marble from the EndPit
  collect_marbles(OppositePit, OpponentPits0, OpponentPits1), % collect the Marbles from the oppononest opposite pits
  PlayerMancala1 is PlayerMancala0 + 1 + MarblesOnOppositePit.

check_if_beatable(_, _, GameState, GameState) :- !.

% Check if the number of moves would be 1 or more (at least 2)
more_turns(Pit, MarblesNumber) :-
  boardSize(BoardSize),
  0 is mod((MarblesNumber-BoardSize+Pit), (2*BoardSize+1)).

% Checks if PlayerPits or OpponentPits is empty (contains only zeroes) and determiens the winner.
game_over(GameState0) :-
  either_side_is_empty(GameState0),
  move_marbles_to_mancalas(GameState0, GameState1),
  winner(GameState1, Winner),
  writeln('\nGame over!'),
  display_board(GameState1),
  display_winner_information(Winner), !.

% Checks if the list contains anything else then 0
pits_are_empty([]).
pits_are_empty([0|T]) :- pits_are_empty(T).

% Checks if either of the players has empty pits
either_side_is_empty(GameState) :-
  GameState = game_state(board(PlayerPits, _), _, _),
  pits_are_empty(PlayerPits).
either_side_is_empty(GameState) :-
  GameState = game_state(_, board(OpponentPits, _), _),
  pits_are_empty(OpponentPits).

% At the end of the game, moves all Marbles from a part of the board to owner's Mancala.
% move_Marbles_to_mancala(Pits, Mancala, FinalMancala)
move_marbles_to_mancala([], Mancala, Mancala) :- !.
move_marbles_to_mancala([H|T], Mancala, FinalMancala) :-
  Mancala1 is Mancala + H,
  move_marbles_to_mancala(T, Mancala1, FinalMancala).

move_marbles_to_mancalas(GameState0, GameState1) :-
  GameState0 = game_state(board(PlayerPits, PlayerMancala0), board(OpponentPits, OpponentMancala0), CurrentPlayer),
  GameState1 = game_state(board([0,0,0,0,0,0], PlayerMancala1), board([0,0,0,0,0,0], OpponentMancala1), CurrentPlayer),
  move_marbles_to_mancala(OpponentPits, OpponentMancala0, OpponentMancala1),
  move_marbles_to_mancala(PlayerPits, PlayerMancala0, PlayerMancala1).

% Returns the Winner
winner(GameState, tie) :-
  GameState = game_state(board(_, PlayerMancala), board(_, OpponentMancala), _),
  PlayerMancala =:= OpponentMancala.

winner(GameState, CurrentPlayer) :-
  GameState = game_state(board(_, PlayerMancala), board(_, OpponentMancala), CurrentPlayer),
  PlayerMancala > OpponentMancala,
  get_player_name(PlayerName), % Get the player's name
  set_winner_name(PlayerName). % Set the player's name as the winner

winner(GameState, NextPlayer) :-
  switch_player(GameState, game_state(_, _, NextPlayer)),
  get_cpu_name(CPUName), % Get the CPU's name
  set_winner_name(CPUName). % Set the CPU's name as the winner



/*
* Functions used for the pit selection, done by the bot.
*/


depth(5).



% List of IDs of all pits on the board
find_best_move(GameState, BestPit, MancalasWeight, BoardsWeight) :-
  GameState = game_state(board(PlayerPits, _), _, _),
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, PlayerPits, [], CorrectPits),
  depth(Depth),
  % CorrectPits - pits that contain any Marbles. Thus, they can be selected in a moves
  select_best_move_from_list(CorrectPits, GameState,
    Depth, minimize, -1, -9999, BestPit, _, MancalasWeight, BoardsWeight).

% all_available_pits([IDs of all pits], OpponentPits, SelectedPits, ResultPits)
% Pits contains a list of all available pits, that can be chosen by the bot
% (which are not empty and place on the bot's board part)
all_available_pits([], [], ResultPits, ResultPits) :- !.
all_available_pits([L|Ls], [Z|Zs], ResultPits, FinalPits) :-
  Z > 0,
  append(ResultPits, [L], ResultPits1),
  !,
  all_available_pits(Ls, Zs, ResultPits1, FinalPits).

all_available_pits([_|Ls], [_|Zs], ResultPits, FinalPits) :-
  all_available_pits(Ls, Zs, ResultPits, FinalPits).

% Function that analyzes all possible moves made by selecting the pits from the lists
select_best_move_from_list([], _, _, _, Pit, Value, Pit, Value, _, _).

select_best_move_from_list([Pit|Pits], GameState0, Depth, Type,
  CurrentPit0, CurrentValue0, BestPit, BestValue, MancalasWeight, BoardsWeight) :-
    GameState0 = game_state(PlayerBoard, _, _),
    % Simulate the move
    marbles_number(Pit, PlayerBoard, MarblesNumber),
    % Check if the move would end in players Mancala - if so, apply another move (if game not over)
    more_turns(Pit, MarblesNumber),
    pass_marbles(Pit, MarblesNumber, GameState0, GameState1),
    !,
    minimax(GameState1, Depth, Type, Type, _, Value, MancalasWeight, BoardsWeight),
    compare(Pit, Value, CurrentPit0, CurrentValue0, CurrentPit1, CurrentValue1),
    select_best_move_from_list(Pits, GameState0, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue, MancalasWeight, BoardsWeight).

select_best_move_from_list([Pit|Pits], GameState0, Depth, Type,
  CurrentPit0, CurrentValue0, BestPit, BestValue, MancalasWeight, BoardsWeight) :-
    GameState0 = game_state(PlayerBoard0, _, _),
    GameState1 = game_state(PlayerBoard1, OpponentBoard1, CurrentPlayer1),
    GameState2 = game_state(OpponentBoard1, PlayerBoard1, CurrentPlayer1),
    % Simulate the move
    marbles_number(Pit, PlayerBoard0, MarblesNumber),
    pass_marbles(Pit, MarblesNumber, GameState0, GameState1),
    !,
    switch_type(Type, NextType),
    minimax(GameState2, Depth, Type, NextType, _, Value, MancalasWeight, BoardsWeight),
    compare(Pit, Value, CurrentPit0, CurrentValue0, CurrentPit1, CurrentValue1),
    select_best_move_from_list(Pits, GameState0, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue, MancalasWeight, BoardsWeight).


switch_type(minimize, maximize).
switch_type(maximize, minimize).


minimax(GameState, Depth, _, NextType, Pit, Value, MancalasWeight, BoardsWeight) :-
  GameState = game_state(board(PlayerPits, _), _, _),
  Depth > 0,
  % Get the list of currently available pits
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, PlayerPits, [], CorrectPits),
  % Prove that list is not empty
  nth0(0, CorrectPits, _),
  Depth1 is Depth - 1,
  select_best_move_from_list(CorrectPits, GameState,
  Depth1, NextType, -1, -9999, Pit, Value, MancalasWeight, BoardsWeight).

  % In case of Depth == 0 or the CorrectPits list is empty
minimax(GameState, _, maximize, _, _, Value, MancalasWeight, BoardsWeight) :-
  evaluate(GameState, Val, MancalasWeight, BoardsWeight),
  Value is Val.

minimax(GameState, _, minimize, _, _, Value, MancalasWeight, BoardsWeight) :-
  evaluate(GameState, Val, MancalasWeight, BoardsWeight),
    Value is (-1) * Val.

evaluate(GameState, Value, _, _) :-
  GameState = game_state(board(PlayerPits, PlayerMancala), board(OpponentPits, OpponentMancala), _),
  sum_list(PlayerPits, PlayerPitsSum),
  sum_list(OpponentPits, OpponentPitsSum),
  Value is 1*(PlayerMancala - OpponentMancala) + 0*(PlayerPitsSum - OpponentPitsSum).


% Compares the give Pit and Value with the Currently best Pit and Value
% and returns the pair with higher Value
compare(_, Value, CurrentPit, CurrentValue, CurrentPit, CurrentValue) :-
  Value =< CurrentValue.
compare(Pit, Value, _, CurrentValue, Pit, Value) :-
  Value > CurrentValue.

play :-
  set_player(1),

  % Asks the user for Player 1's name
  nl,
  write('Player 1, enter your name: '),
  read_string(user_input, "\n", "\r", _, PlayerName),
  set_player_name(PlayerName),

  % Asks the user for Player 2's name
  nl,
  write('Player 2, enter your name: '),
  read_string(user_input, "\n", "\r", _, CPUName),
  set_cpu_name(CPUName),

  play(1, 0).

play_cpu :-
    % Loop until valid input is provided
    % repeat,
    (
        % Asks the user for CPU difficulty
        nl,
        write('Enter CPU difficulty (0 for Easy, 1 for Hard): '),
        read_string(user_input, "\n", "\r", _, DifficultyStr),
        atom_number(DifficultyStr, Difficulty), % Convert input to a number

        % Validate the input
        (Difficulty =:= 0 ; Difficulty =:= 1)
    ),
    
    ( Difficulty =:= 1 ->
        set_player(0)
    ; Difficulty =:= 0 ->
        set_player(2)
    ),

    % Asks the user for Player name
    nl,
    write('Enter your name: '),
    read_string(user_input, "\n", "\r", _, PlayerName),
    set_player_name(PlayerName),

    % Generates a random name for the CPU
    cpu_names(Name),
    random_member(CPUName, Name), % Picks a random CPU name from the list
    set_cpu_name(CPUName),

    play(1, 0).


play(MancalasWeight, BoardsWeight) :-
  initialize(GameState),
  play(GameState, MancalasWeight, BoardsWeight).

% Initialize an empty mancala board with 6 pits (with 4 Marbles in each of them) per player
% side and with 2 empty Mancalas.
initialize(game_state(board([4,4,4,4,4,4], 0), board([4,4,4,4,4,4], 0), player)).

% Check if any of the players won. If not then:
% 1. Select a start pit to take Marbles to pass from.
% 2. Make the move (pass Marbles to consecutive pits). (? Display the board after the move)
% 3. Switch player and play recursively.
play(GameState0, MancalasWeight, BoardsWeight) :-
  select_and_make_move(GameState0, GameState1, MancalasWeight, BoardsWeight),
  switch_player(GameState1, GameState2),
  !,
  play(GameState2, MancalasWeight, BoardsWeight).

select_and_make_move(GameState0, GameState1, MancalasWeight, BoardsWeight) :-
  not(game_over(GameState0)),
  display_board(GameState0),
  select_pit(GameState0, Pit, MancalasWeight, BoardsWeight),
  make_move(Pit, GameState0, GameState1, MancalasWeight, BoardsWeight).

select_pit(GameState, Pit, MancalasWeight, BoardsWeight) :-
    GameState = game_state(PlayerBoard, _, player),
    repeat, % Allows retrying until a valid pit is selected
    nl, writeln(['Select pit (1 - 6)']), 
    read_pit(Pit0, marbles_number, PlayerBoard), % Read pit number from the user
    % Convert Pit0 to the correct number (0-5)
    Pit is Pit0 - 1,
    marbles_number(Pit, PlayerBoard, MarblesNumber), % Get the number of Marbles in the selected pit
    (MarblesNumber > 0 -> % If MarblesNumber is greater than 0
        true % Proceed
    ;   write('Please select a pit with a marble!'), nl, % Print a message indicating that the selected pit is empty
        fail % Fail to force retrying
    ).


select_pit(GameState, Pit, MancalasWeight, BoardsWeight) :-
    GameState = game_state(CurrentPlayerBoard, _, _),
    get_player(Num),
    (Num = 1 ->
        repeat,
        nl, writeln(['Select pit (1 - 6)']), 
        read_pit(Pit0, marbles_number, CurrentPlayerBoard),
        Pit is Pit0 - 1,
        marbles_number(Pit, CurrentPlayerBoard, MarblesNumber),
        (MarblesNumber > 0 ->
            true
        ;   write('Please select a pit with a marble!'), nl,
            fail
        )
    ; Num = 0 ->
        bot_hard_pit(GameState, Pit, MancalasWeight, BoardsWeight)
    ; Num = 2 ->
        bot_easy_pit(GameState, Pit, MancalasWeight, BoardsWeight)
    ).

bot_hard_pit(GameState, Pit, MancalasWeight, BoardsWeight) :-
    writeln(['Opponent\'s turn']),
    find_best_move(GameState, Pit, MancalasWeight, BoardsWeight),
    % Convert Pit to the correct number (1-6) just for display
    Pit_display is Pit + 1,
    write('Opponent selected: '),
    writeln(Pit_display).

bot_easy_pit(GameState, Pit, MancalasWeight, BoardsWeight) :-
    writeln(['Opponent\'s turn']),
    select_random_move(GameState, Pit, MancalasWeight, BoardsWeight), % Call the new predicate to select a random legal move
    % Convert Pit to the correct number (1-6) just for display
    Pit_display is Pit + 1,
    write('Opponent selected: '),
    writeln(Pit_display).

select_random_move(GameState, Pit, _, _) :-
    GameState = game_state(board(PlayerPits, _), _, _),
    boardSize(BoardSize),
    consecutive(BoardSize, InitialList),
    all_available_pits(InitialList, PlayerPits, [], CorrectPits),

    % Choose a random correct pit
    length(CorrectPits, Length),
    random(0, Length, RandomIndex),
    nth0(RandomIndex, CorrectPits, Pit).

% make_move(Pit, GameState0, GameState2)
% Pit - the ID of the selected start pit (Pits are numbered from 0 to 5).
% PlayerBoard, OpponentBoard - initial state of the game board
% NewPlayerBoard, NewOpponentBoard - final state of game board, after distributing
% the Marbles
% Check if finished in Mancala and whether there should be another move for this player
make_move(Pit, GameState0, GameState2, MancalasWeight, BoardsWeight) :-
  GameState0 = game_state(PlayerBoard0, _, _),
  marbles_number(Pit, PlayerBoard0, MarblesNumber),
  % Check if the move would end in players Mancala - if so, apply another move (if game not over)
  (more_turns(Pit, MarblesNumber) ->
    writeln('Extra turn!'),
    get_player_name(PlayerName),
    get_cpu_name(CPUName),
    set_player_name(CPUName),
    set_cpu_name(PlayerName),
    pass_marbles(Pit, MarblesNumber, GameState0, GameState1),
    select_and_make_move(GameState1, GameState2, MancalasWeight, BoardsWeight)
  ;
    pass_marbles(Pit, MarblesNumber, GameState0, GameState2)
  ).

make_move(Pit, GameState0, GameState1, _, _) :-
  GameState0 = game_state(PlayerBoard0, _, _),
  marbles_number(Pit, PlayerBoard0, MarblesNumber),
  pass_marbles(Pit, MarblesNumber, GameState0, GameState1).