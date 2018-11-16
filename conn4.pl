#!/usr/bin/env swipl
% -------------------------------------------------------------------------
%   Four-In-A-Row in Prolog
%   =======================
%  Programmer   - Yuval Gitlitz, Daniel Orbach
%  File Name    - conn4.pl
%  Description  - This program play the classic well-known childhood game 
%                 called "connect 4" or "4 in a row".
%                 The program displays a graphical interface, aswell as an AI
%                 which is a cimputer playing againt a human player.
%  Input        - a series of numeric (0-7) key strokes 
%                 all input is presented received through interpreter window
%  Ouptut       - interaction with a graphical interface
%                 all output is presented on the graphical interface
% -------------------------------------------------------------------------

% program entry point
main:-
    current_prolog_flag(argv, [Arg]),
    atom_number(Arg, Depth),
    b_setval(max_depth, Depth),
    empty_board(6, 7, Board0),
    interact(x, Board0).


% -------------------------------------------------------------------------
%  The game board is represented as a list of columns.
% 
%  Each column is a term col(Num,Free,TP,TN,Ps), where:
%   Num: column number
%   Free: yes/no, whether a piece can be placed "on top" (at the end)
%   TP: Color of topmost piece
%   TN: max. number of consecutive topmost pieces of same colour
%   Ps: Pieces in this column
% 
%  Each piece is one of:
%   - : empty cell
%   x : piece of player x
%   o : piece of player o
% -------------------------------------------------------------------------

% is column empty
empty(-).

% initialize board Board in size N x M
empty_board(N, M, Board) :-
        length(Board, M),
        length(Es, N),
        maplist(empty, Es),
        foldl(column(Es), Board, 1, _).

% initialize an empty column. N is the next piece
column(Empty, col(N0,yes,empty,0,Empty), N0, N) :-
        N is N0 + 1.

% Check if Player won on the given Board
win(Player, Board) :-
        (   member(col(_,_,Player,N,_), Board), N >= 4;
        un_col(Board, Board1),
        (   four_in_a_row(Board1, Player)
        ;   diagonal(Board1, Player)
        )).

% Hueristic function on Board. Return the result on Score
hueristic(Score, Board):-
	max_row(XL, Board, x),
	max_row(OL, Board, o),
	Score is XL - OL.

% Return the longest row Player has on Board. The result is stored on Length
max_row(Length, Board, Player):-
        un_col(Board, UBoard),
	((member(col(_,_,Player,3,_), Board);
	three_in_a_row(UBoard, Player);
	three_diagonal(UBoard, Player)) -> Length = 3;
	(member(col(_,_,Player,2,_), Board);
	two_in_a_row(UBoard, Player);
	two_diagonal(UBoard, Player)) -> Length = 2;
	Length=1).
% true iff player has 3 length row
three_in_a_row([Col1,Col2,Col3|Cs], Player) :-
        (   three_in_a_row(Col1, Col2, Col3, Player)
        ;   three_in_a_row([Col2,Col3|Cs], Player)
        ).

three_in_a_row([C1|Cs1], [C2|Cs2], [C3|Cs3],P) :-
        \+ empty(C1),
        \+ empty(C2),
        \+ empty(C3),
        (   C1 == P, C2 == P, C3 == P
        ;   three_in_a_row(Cs1, Cs2, Cs3, P)
        ).

% check 3 length diagonal row
three_diagonal(Board, Player) :-
        Board = [_,_,_,_|_],
        (   three_diagonal_down(Board, Player)
        ;   three_diagonal_up(Board, Player)
        ;   Board = [_|Rest],
            three_diagonal(Rest, Player)
        ).

three_diagonal_down([Col1,Col2,Col3|_], Player) :-
        Col2 = [_|Rot2],
        Col3 = [_,_|Rot3],
        three_in_a_row(Col1, Rot2, Rot3, Player).

three_diagonal_up([Col1,Col2,Col3|_], Player) :-
        Col1 = [_,_,_|Rot1],
        Col2 = [_,_|Rot2],
        Col3 = [_|Rot3],
        three_in_a_row(Rot1, Rot2, Rot3, Player).

% true iff player has 2 length row
two_in_a_row([Col1,Col2|Cs], Player) :-
        (   two_in_a_row(Col1, Col2, Player)
        ;   two_in_a_row([Col2|Cs], Player)
        ).

two_in_a_row([C1|Cs1], [C2|Cs2], P) :-
        \+ empty(C1),
        \+ empty(C2),
        (   C1 == P, C2 == P
        ;   two_in_a_row(Cs1, Cs2, P)
        ).

% check 2 length diagonal row
two_diagonal(Board, Player) :-
        Board = [_,_,_,_|_],
        (   two_diagonal_down(Board, Player)
        ;   two_diagonal_up(Board, Player)
        ;   Board = [_|Rest],
            two_diagonal(Rest, Player)
        ).

two_diagonal_down([Col1,Col2|_], Player) :-
        Col2 = [_|Rot2],
        two_in_a_row(Col1, Rot2, Player).

two_diagonal_up([Col1,Col2|_], Player) :-
        Col1 = [_,_,_|Rot1],
        Col2 = [_,_|Rot2],
        two_in_a_row(Rot1, Rot2, Player).


% convert col list to a list containing only the columns content
un_col([], []).
un_col([col(_,_,_,_,Cs)|Rest], [Cs|Css]) :-  un_col(Rest, Css).

% true iff player has 3 length row
four_in_a_row([Col1,Col2,Col3,Col4|Cs], Player) :-
        (   four_in_a_row(Col1, Col2, Col3, Col4, Player)
        ;   four_in_a_row([Col2,Col3,Col4|Cs], Player)
        ).

four_in_a_row([C1|Cs1], [C2|Cs2], [C3|Cs3], [C4|Cs4], P) :-
        \+ empty(C1),
        \+ empty(C2),
        \+ empty(C3),
        \+ empty(C4),
        (   C1 == P, C2 == P, C3 == P, C4 == P
        ;   four_in_a_row(Cs1, Cs2, Cs3, Cs4, P)
        ).


% true iff board has 4 length diagonal row
diagonal(Board, Player) :-
        Board = [_,_,_,_|_],
        (   diagonal_down(Board, Player)
        ;   diagonal_up(Board, Player)
        ;   Board = [_|Rest],
            diagonal(Rest, Player)
        ).

diagonal_down([Col1,Col2,Col3,Col4|_], Player) :-
        Col2 = [_|Rot2],
        Col3 = [_,_|Rot3],
        Col4 = [_,_,_|Rot4],
        four_in_a_row(Col1, Rot2, Rot3, Rot4, Player).

diagonal_up([Col1,Col2,Col3,Col4|_], Player) :-
        Col1 = [_,_,_|Rot1],
        Col2 = [_,_|Rot2],
        Col3 = [_|Rot3],
        four_in_a_row(Rot1, Rot2, Rot3, Col4, Player).

% Insert piece to P, by Player
insert_piece_([P|Ps], Player, Is, Free) :-
        (   empty(P) ->
            Is = [Player|Ps],
            (   Ps = [] ->  Free = no
            ;   Free = yes
            )
        ;   Is = [P|Rest],
            insert_piece_(Ps, Player, Rest, Free)
        ).
% Let the computer find what to do
play_column([Col0|Cols0], Column, Player, [Col|Cols]) :-
        Col0 = col(CN0,_,TP0,TN0,Cs0),
        (   CN0 = Column ->
            insert_piece_(Cs0, Player, Cs, Free),
            Cols = Cols0,
            (   TP0 == Player ->
                TN is TN0 + 1
            ;   TN = 1
            ),
            Col = col(CN0,Free,Player,TN,Cs)
        ;   Col = Col0,
            play_column(Cols0, Column, Player, Cols)
        ).

%  -------------------------------------------------------------------------
%  Minimax algorithm with alpha-beta pruning
%
%   x is the maximizing player. We rate each column that x can (still) choose 
%   by rating the best move that o can make in response, and then take one of
%   the columns where this value attains its maximum.
%   In other words, the best move of x makes it hardest for o.
%   The "best move o could do" is computed analogously, by making it hard for x.
%
%   Each possible move is simply represented by its column number.
%   The score of each move is:
%     - positive if x wins by this move
%     - zero if no decision is reached yet
%     - negative if o wins.
%
%   Ties are broken by picking a random move among the best ones, therefore
%   the game typically differs from run to run.
% -------------------------------------------------------------------------
% return the oponent of a player
opponent(x, o).
opponent(o, x).

% set x as the maximizer
max_player(x).

% true if the column is empty
free_column(col(_,yes,_,_,_)).

% get the column number
column_number(col(Num,_,_,_,_), Num).

% return all the available column for the next move
possible_columns(Cols0, Cs) :-
        include(free_column, Cols0, Cols),
        maplist(column_number, Cols, Cs).

% find best move for Player on the given board using alpha beta pruning
play(Player, Board0, Move) :-
        possible_columns(Board0, Columns),
        b_getval(max_depth, Depth),
        Alpha is -Depth - 1,
        Beta is Depth + 1,
        moves_with_scores(Columns, Depth, Alpha, Beta, Player, Board0, SMs),
        best_move(Player, SMs, Move).

key_eq(G, G-_).

% Choose a move randomly from SMs with the best score
best_move(Player, SMs, Move) :-
        best_score(SMs, Player, Score),
        include(key_eq(Score), SMs, BestMoves),
        length(BestMoves, LC),
        Index is random(LC),
        nth0(Index, BestMoves, _-Move).

% alpha beta pruning for the best move
moves_with_scores([], _, _, _, _, _, []).
moves_with_scores([M|Ms], Depth, Alpha0, Beta0, Player, Board0, [Score-M|SMs]) :-
        move_score(Depth, Alpha0, Beta0, Player, Board0, M, Score),
        (   max_player(Player) ->
            Alpha is max(Alpha0, Score),
            Beta is Beta0
        ;   Alpha is Alpha0,
            Beta is min(Beta0, Score)
        ),
        (   Beta < Alpha -> SMs = []
        ;   moves_with_scores(Ms, Depth, Alpha, Beta, Player, Board0, SMs)
        ).

move_score(Depth, Alpha, Beta, Player, Board0, Move, Score) :-
        (   Depth is 0 -> hueristic(Score, Board0)
        ;   play_column(Board0, Move, Player, Board1),
            (   win(Player, Board1) ->
                (   max_player(Player) ->
                    Score is 99 + Depth % favor early wins
                ;   Score is -99 - Depth
                )
            ;   possible_columns(Board1, Moves),
                (   Moves == [] ->
                    Score is 0
                ;   D1 is Depth - 1,
                    opponent(Player, Opp),
                    moves_with_scores(Moves, D1, Alpha, Beta, Opp, Board1, SMs),
                    best_score(SMs, Opp, Score)
                )
            )
        ).

% filter the moves with the best score
best_score(SMs0, Player, Score) :-
        keysort(SMs0, SMs),
        (   max_player(Player) ->
            last(SMs, Score-_)
        ;   SMs = [Score-_|_]
        ).

% handle user input
user_input(Board, Char) :-
        get_single_char(Char0),
        (   Char0 == (0'0) -> Char = c
        ;   between(0'1, 0'7, Char0) ->
            Char1 is Char0 - 0'0,
            (   play_column(Board, Char1, x, _) -> Char = Char1
            ;   user_input(Board, Char)
            )
        ;   user_input(Board, Char)
        ).

% get C&C from stdin and handle it
interact(Player, Board0) :-
        (   \+ play(Player, Board0, _) -> format("draw\n")
        ;   
	    user_input(Board0, Char),
            (   Char == c ->
                play(Player, Board0, Column)
            ;   Column = Char
            ),
            play_column(Board0, Column, Player, Board1),
            format("/~w ~w drop\n", [Player,Column]),
            (   win(Player, Board1) ->
                format("/~w wins\n", [Player])
            ;   opponent(Player, Opp),
                interact(Opp, Board1)
            )
        ).

:- main.
