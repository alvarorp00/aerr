% Space of 4x3 to be filled with two L and one O figures:
%  - Rows: 1..3
%  - Columns: 1..4

% Board

int: x_lim = 4;
int: y_lim = 3;

set of int: COLUMNS = 1..x_lim;
set of int: ROWS = 1..y_lim;

% Figures

int : L = 1;
int : O = 2;

int : L1 = 1;
int : L2 = 2;
int : O1 = 3;

set of int: FIGURES = 1..2;  % L,O
set of int: BLOCKS = 1..3;  % L1,L2,O1

% Figures
array[BLOCKS] of var FIGURES: BlockType = [L1: L, L2: L, O1: O];

% end

array[COLUMNS, ROWS] of var BLOCKS: board;  % the idea is to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

array[BLOCKS] of var COLUMNS: x;  % x coordinate of the block
array[BLOCKS] of var ROWS: y;  % y coordinate of the block

% Constraints

constraint forall(b in BLOCKS)(
    if BlockType[b] = L then
        (board[x[b], y[b]] = b /\ board[x[b], y[b]+1] = b /\ board[x[b], y[b]+2] = b /\ board[x[b]+1, y[b]+2] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b]+1, y[b]] = b /\ board[x[b]+2, y[b]] = b /\ board[x[b]+2, y[b]-1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b]-1, y[b]] = b /\ board[x[b]-2, y[b]] = b /\ board[x[b]-2, y[b]+1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b], y[b]-1] = b /\ board[x[b], y[b]-2] = b /\ board[x[b]-1, y[b]-2] = b)
    else % BlockType[b] = O
        (board[x[b], y[b]] = b /\ board[x[b]+1, y[b]] = b /\ board[x[b], y[b]+1] = b /\ board[x[b]+1, y[b]+1] = b)
    endif
);

% Blocks are not placed twice
constraint forall(b in BLOCKS)(
    sum(i in 1..4, j in 1..3)(board[i,j] = b) = 4  % 4-sized blocks
);


ann: search_ann;
search_ann = int_search(board, first_fail, indomain_median, complete);
solve :: search_ann satisfy;

% solve satisfy;

% Show board (4x3 grid)
output ["Board: [" ++ "\n" ++
            " " ++ show(board[1,1]) ++ " " ++  show(board[1,2]) ++ " " ++  show(board[1,3]) ++ "\n"
            ++ " " ++  show(board[2,1]) ++ " " ++  show(board[2,2]) ++ " " ++  show(board[2,3]) ++ "\n"
            ++ " " ++  show(board[3,1]) ++ " " ++  show(board[3,2]) ++ " " ++  show(board[3,3]) ++ "\n"
            ++ " " ++  show(board[4,1]) ++ " " ++  show(board[4,2]) ++ " " ++  show(board[4,3]) ++ "\n]" ++ "\n"];