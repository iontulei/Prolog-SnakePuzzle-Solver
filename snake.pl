:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true).
:- [tests2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------|  Main  |-------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The snake/4 predicate is the main predicate that solves the puzzle.
% It devises a solution by applying constraints from the predicates:
% countHeads, checkRowClues, checkColClues, nonTouching, countNeighbors, snakeConnected.

snake(RowClues, ColClues, Grid, Solution)
:- copyGrid(Grid,Solution)
, countHeads(Solution)
, checkRowClues(Solution,RowClues)
, checkColClues(Solution,ColClues)
, nonTouching(Solution) % snake cannot touch itself
, countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
, snakeConnected(Solution) % snake must be connected
.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------|  Copy Grid  |-------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The copyGrid/2 predicate makes a copy of the original grid,
% replacing -1 with either 0 or 2.
copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).


% The copyRow/2 predicate copies everything except the -1 cells.
% In here we apply constraints to the cells of the new that are not yet bounded (be 0 or 2).
copyRow([],[]).
copyRow([-1|R],[V|S]) :- (V #= 0 #\/ V #= 2), copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :-  (Clue #\= -1), copyRow(R,S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------|  Count Heads  |-------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ensure that the grid has only 2 heads (1s).
% We make use of the flatten/2 predicate which merges nested lists.
countHeads(Grid) :-
    flatten(Grid, FlatGrid),
    count_ones(FlatGrid, N), N #= 2.


% Helper predicate, count_ones/2, that recursively counts ones in a list.
count_ones([], 0).
count_ones([1|Rest], N) :- count_ones(Rest, N1), N is N1 + 1, !.
count_ones([_|Rest], N) :- count_ones(Rest, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------|  Grid Clues  |-------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The checkValue/2 predicate checks whether in an array (first parameter)
% there are the same number of snake pieces as indicated by the clue 'Value' (second parameter).
checkValue([], 0).
checkValue([0|Rest], N) :- checkValue(Rest, N).
checkValue([Value|Rest], N) :- 
    (Value = 2; Value = 1),
    checkValue(Rest, NewValue),
    N is (NewValue + 1).


% The checkRowClue/2 receives two parameters (Grid/Solution and RowClues), 
% and then goes iteratively through all of the rows of the solution and checks 
% if the number of pieces of the snake is the same as the value of the RowClues, 
% using checkValue, which receives a row and the Value from RowClues
checkRowClues([], []).
checkRowClues([_|List2], [-1|List]) :- checkRowClues(List2, List).
checkRowClues([Row|List2], [Value|List]) :- 
    checkValue(Row, Value), 
    checkRowClues(List2, List).


% The checkColClues/2 predicate receives two parameters (Grid/Solution and ColClues), 
% and then at first splits the grid/solution in a column and the rest of the grid 
% and then using the column checks, as in checkRowClues, using checkValue that the number of pieces 
% of the snake in that array coincides with the clue and then recursively checks the remaining of the grid.
checkColClues(_, []).
checkColClues(Table, [-1|Rest]) :- 
    splitTable(Table, _, RestTable), checkColClues(RestTable, Rest).

checkColClues(Table, [Value|Rest]) :-
    splitTable(Table, Col, RestTable),
    checkValue(Col, Value), 
    checkColClues(RestTable, Rest).


% The splitTable/3 predicate receives three parameters (Grid/Solution, the first column, The rest of the grid).
% The predicate checks or, splits the grid (2D array) in a list, which represents the first column, 
% by taking the first element of each row, and the rest of the grid, 
% which is the remaining of the row arrays without the first element.
splitTable([], [], []).
splitTable([[First|Rest]|Rows], [First|Col], [Rest|T]) :- splitTable(Rows, Col, T). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------|  Non Touching  |-------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The nonTouching/1 predicate recives a solution and checks whether it has touching pattern.
nonTouching(Solution) :- splitSquare(Solution), !.


% 0 1  |  1 0  |  1 1
% 1 0  |  0 1  |  1 1
% The patternTouching/2 represents the patterns in which the snake is considered touching.
patternTouching([[0, X], [Y, 0]]) :- \+ (X = 0), \+ (Y = 0), !.
patternTouching([[X, 0], [0, Y]]) :- \+ (X = 0), \+ (Y = 0), !.
patternTouching([[X, Y], [Z, W]]) :- \+ (X = 0), \+ (Y = 0), \+ (W = 0), \+ (Z = 0), !.


% The splitSquare/1 is a predicate which takes the solution and then takes the first two rows 
% of the solution and passes them to splitSquareRow/1, which in term is going to decide 
% if the rows satisfy the nonTouching pattern, and then iteratively checks the remaning of the rows using the same method.
splitSquare([]).
splitSquare([_]).
splitSquare([X, Y|Row]) :- splitSquareRow([X, Y]), splitSquare([Y|Row]).


% The splitSquareRow/1 predicate receives two arrays. The predicate then takes 2 elements 
% from each of the arrays and checks whether they have the touching pattern, 
% which they should not have, and then iteratively checking the rest of the array for the same pattern.
splitSquareRow([[], []]).
splitSquareRow([[_], [_]]).
splitSquareRow([[X, Y|Row], [X1, Y1|Row1]]) :- \+ patternTouching([[X, Y], [X1, Y1]]), splitSquareRow([[Y|Row], [Y1|Row1]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------|  Count Neighbors  |-------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate ensures that heads have 1 neighbor and midpoints have 2 neighbors.
% Neighbors: N, E, S, W.
countNeighbors(Solution) :-
    extend_grid(Solution, NeighborGrid),
    check_neighbors_grid(NeighborGrid).


% Extend the grid with zeroes from the sides and from the top and bottom.
% This is done to make the check for neighbors possible.
extend_grid(OldGrid, NewGrid) :-
    transpose(OldGrid, TransGrid),
    extend_grid_rows(TransGrid, RowTransGrid),
    transpose(RowTransGrid, RowGrid),
    extend_grid_rows(RowGrid, NewGrid).


% Extend each row of the grid with zeros.
extend_grid_rows([], []).
extend_grid_rows([Row|Rest], [NewRow|NewRest]) :-
    extend_row(Row, NewRow),
    extend_grid_rows(Rest, NewRest).


% Extend a single row with zeros.
extend_row(OldRow, NewRow) :- 
    append([0|OldRow], [0], NewRow), !.


% Check neighbors of all rows (except the 0-padded rows).
check_neighbors_grid([X, Y, Z]) :-
    check_neighbors_rows(X, Y, Z).

check_neighbors_grid([X, Y, Z | Rest]) :-
    check_neighbors_rows(X, Y, Z),
    check_neighbors_grid([Y, Z | Rest]).


% Check the neighbors of each cell in the grid.
% The predicate receives 3 rows as arguments to be able to extract the neighbors.
check_neighbors_rows([_, N, _], [W, M, _], [_, S, _]) :- 
    check_neighbors_pattern(M, N, 0, S, W).

check_neighbors_rows([_, N, A3 | RowA], [W, M, E | RowB], [_, S, C3 | RowC]) :-
    check_neighbors_pattern(M, N, E, S, W),
    check_neighbors_rows([N, A3 | RowA], [M, E | RowB], [S, C3 | RowC]).


% Check if the sum of neighbors equals the Piece value.
check_neighbors_pattern(0, _, _, _, _).
check_neighbors_pattern(Piece, N, E, S, W) :- 
    1 #=< Piece,
    count_cell(N, X1),
    count_cell(E, X2),
    count_cell(S, X3),
    count_cell(W, X4),
    Piece #= X1 + X2 + X3 + X4.


% Check if a cell value is valid (0, 1, or 2).
count_cell(2, 1).
count_cell(0, 0).
count_cell(1, 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------|  Snake Connected  |-------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ensure that the snake is fully connected.
% Firstly, find one of the snake heads and replace it with 2 because
% the flood_fill/4 predicate stops when it encounters a 1.
% Secondly, call the flood_fill/4 predicate to start replacing the snake body parts.
% Finaly, flatten the grid and check if no more 1s and 2s are present.
snakeConnected(Solution) :-
    find_start(Solution, X, Y),
    replace_in_grid(Solution, X, Y, 2, TmpGrid),
    flood_fill(TmpGrid, X, Y, NewGrid),
    flatten(NewGrid, FlatGrid),
    \+ (member(Cell, FlatGrid),
    (Cell #= 1 #\/ Cell #= 2)), !.


% Find the coordinates of one of the snake heads.
% Y - row no in grid; X - cell no in row.
find_start(Grid, X, Y) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, 1), !.


% Replace all connected snake pieces starting from (X, Y) with 3.
flood_fill(Grid, X, Y, Grid) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, Cell),
    (Cell #= 0), !.

flood_fill(Grid, X, Y, NewGrid) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, Cell),
    (Cell #= 2),
    replace_in_grid(Grid, X, Y, 3, TmpGrid),
    propagate(TmpGrid, X, Y, NewGrid), !.

flood_fill(Grid, X, Y, NewGrid) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, Cell),
    (Cell #= 1),
    replace_in_grid(Grid, X, Y, 3, NewGrid), !.

flood_fill(Grid, _, _, Grid).


% Replace the cell at the given (X, Y) position with a new value and return the new grid.
replace_in_grid(Grid, X, Y, NewVal, NewGrid) :-
    nth0(Y, Grid, Row, RestRows),
    nth0(X, Row, _, RestCells),
    nth0(X, NewRow, NewVal, RestCells),
    nth0(Y, NewGrid, NewRow, RestRows).


% Propagate the flood_fill/4 to adjacent cells (N, E, S, W) using the move/4 predicate.
% If move/4 fails in one direction, we use the state of the grid before calling move/4.
% If move/4 succeeds, take the resulting grid with the updated cells and pass it to
% to the next move/4 or return the final grid.
propagate(Grid, X, Y, NewGrid) :-
    (Y1 is Y - 1, move(Grid , X, Y1, Grid1  ); Grid1   = Grid ), % N
    (X1 is X + 1, move(Grid1, X1, Y, Grid2  ); Grid2   = Grid1), % E
    (Y2 is Y + 1, move(Grid2, X, Y2, Grid3  ); Grid3   = Grid2), % S
    (X2 is X - 1, move(Grid3, X2, Y, NewGrid); NewGrid = Grid3). % W


% Move to an adjacent cell and 'continue' the flood_fill/4.
% This predicate fails when the cell is not 1 or 2,
% or when X or Y are outside the grid bounds.
move(Grid, X, Y, NewGrid) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, Cell),
    (Cell #= 1 #\/ Cell #= 2),
    flood_fill(Grid, X, Y, NewGrid).