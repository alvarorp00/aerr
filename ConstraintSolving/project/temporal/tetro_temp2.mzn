% Space of 4x3 to be filled with two L and one O figures

include "alldifferent.mzn";

% Board

int: x_lim = 4;
int: y_lim = 4;

set of int: COLUMNS = 1..x_lim;
set of int: ROWS = 1..y_lim;

int: L = 1;
int: S = 2;
int: I = 3;

% Blocks

int: L1 = 1;
int: L2 = 2;
int: S1 = 3;
int: I1 = 4;

% BlockType

set of int: FIGURES = 1..3;  % L,S,I
set of int: BLOCKS = 1..4;  % L1,L2,S1,I1

% Figures
array[BLOCKS] of var FIGURES: BlockType = [L,L,S,I];

% blocktype constraints

constraint BlockType[L1] = L;
constraint BlockType[L2] = L;
constraint BlockType[S1] = S;
constraint BlockType[I1] = I;

% end

array[ROWS, COLUMNS] of var BLOCKS: board;  % the idea is to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

% array[Block] of var COLUMNS: x;  % x coordinate of the block
% array[Block] of var ROWS: y;  % y coordinate of the block

% columns and rows

array[BLOCKS] of var COLUMNS: x; % x coordinate of the block
array[BLOCKS] of var ROWS: y; % y coordinate of the block

% Valid coordinates for each block

constraint (board[y[I1], x[I1]] = I1 /\ board[y[I1], x[I1]+1] = I1 /\ board[y[I1], x[I1]+2] = I1 /\ board[y[I1], x[I1]+3] = I1) \/
            (board[y[I1], x[I1]] = I1 /\ board[y[I1]+1, x[I1]] = I1 /\ board[y[I1]+2, x[I1]] = I1 /\ board[y[I1]+3, x[I1]] = I1);

constraint (board[y[L1], x[L1]] = L1 /\ board[y[L1], x[L1]+1] = L1 /\ board[y[L1], x[L1]+2] = L1 /\ board[y[L1]-1, x[L1]+2] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1]+1, x[L1]] = L1 /\ board[y[L1]+2, x[L1]] = L1 /\ board[y[L1]+2, x[L1]+1] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1]-1, x[L1]] = L1 /\ board[y[L1]-2, x[L1]] = L1 /\ board[y[L1]-2, x[L1]-1] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1], x[L1]-1] = L1 /\ board[y[L1], x[L1]-2] = L1 /\ board[y[L1]+1, x[L1]-2] = L1);

constraint (board[y[L2], x[L2]] = L2 /\ board[y[L2], x[L2]+1] = L2 /\ board[y[L2], x[L2]+2] = L2 /\ board[y[L2]-1, x[L2]+2] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2]+1, x[L2]] = L2 /\ board[y[L2]+2, x[L2]] = L2 /\ board[y[L2]+2, x[L2]+1] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2]-1, x[L2]] = L2 /\ board[y[L2]-2, x[L2]] = L2 /\ board[y[L2]-2, x[L2]-1] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2], x[L2]-1] = L2 /\ board[y[L2], x[L2]-2] = L2 /\ board[y[L2]+1, x[L2]-2] = L2);

constraint (board[y[S1], x[S1]] = S1 /\ board[y[S1], x[S1]+1] = S1 /\ board[y[S1]+1, x[S1]+1] = S1 /\ board[y[S1]+1, x[S1]+2] = S1) \/
            (board[y[S1], x[S1]] = S1 /\ board[y[S1]+1, x[S1]] = S1 /\ board[y[S1]+1, x[S1]+1] = S1 /\ board[y[S1]+2, x[S1]+1] = S1);

% Pieces can't appear twice

constraint all_different(x);
constraint all_different(y);

% Solve

% ann: search_ann;
% search_ann = int_search(board, first_fail, indomain);
% solve :: search_ann satisfy;

solve :: int_search(board, first_fail, indomain_min)
      satisfy;

% Show board (4x4 grid)
output ["Board: [" ++ "\n" ++
             " " ++ show(board[1,1]) ++ " " ++  show(board[1,2]) ++ " " ++  show(board[1,3]) ++ " " ++  show(board[1,4]) ++ "\n"
            ++ " " ++  show(board[2,1]) ++ " " ++  show(board[2,2]) ++ " " ++  show(board[2,3]) ++ " " ++ show(board[2,4]) ++ "\n"
            ++ " " ++  show(board[3,1]) ++ " " ++  show(board[3,2]) ++ " " ++  show(board[3,3]) ++ " " ++ show(board[3,4]) ++ "\n"
            ++ " " ++  show(board[4,1]) ++ " " ++  show(board[4,2]) ++ " " ++  show(board[4,3]) ++ " " ++ show(board[4,4]) ++ "\n" ++ "]"];