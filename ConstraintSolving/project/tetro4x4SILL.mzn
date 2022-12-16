% Space of 4x3 to be filled with two L and one O figures:
%  - Rows: 1..3
%  - Columns: 1..4

% Board

int: x_lim = 4;
int: y_lim = 4;

set of int: COLUMNS = 1..x_lim;
set of int: ROWS = 1..y_lim;

% Figures

enum Figure = {L,S,I};
enum Block = {L1,L2,S1,I1};

array[Block] of Figure: BlockType = [L,L,S,I];

% blocktype constraints

constraint BlockType[L1] = L;
constraint BlockType[L2] = L;
constraint BlockType[S1] = S;
constraint BlockType[I1] = I;

% end

set of Block: BLOCKS = {I1,L1,L2,S1};

% end

array[COLUMNS, ROWS] of var BLOCKS: board;  % the idea is to distinguish between the blocks of the board (throught the block number)

array[BLOCKS] of var COLUMNS: x;  % x coordinate of the block
array[BLOCKS] of var ROWS: y;  % y coordinate of the block

% Constraints

% The piece I1 can be placed on the top, bottom, left or right side of the board as if we place it in other place
% then we will not be able to place the other pieces (L1, L2, S1) bcs they will overlap with I1 or do not fit in the board
var bool: I1_top = forall(i in 1..x_lim)(
    board[i,1] = I1
);

var bool: I1_bottom = forall(i in 1..x_lim)(
    board[i,4] = I1
);

var bool: I1_left = forall(j in 1..y_lim)(
    board[1,j] = I1
);

var bool: I1_right = forall(j in 1..y_lim)(
    board[4,j] = I1
);

constraint I1_top \/ I1_bottom \/ I1_left \/ I1_right;

constraint if I1_top then y[L1] >= 2 /\ y[L2] >= 2 /\ y[S1] >= 2 endif;
constraint if I1_bottom then y[L1] <= 3 /\ y[L2] <= 3 /\ y[S1] <= 3 endif;
constraint if I1_left then x[L1] >= 2 /\ x[L2] >= 2 /\ x[S1] >= 2 endif;
constraint if I1_right then x[L1] <= 3 /\ x[L2] <= 3 /\ x[S1] <= 3 endif;

% The idea is to use the board array to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

constraint forall(b in BLOCKS)(
    if BlockType[b] = L then
        (board[x[b], y[b]] = b /\ board[x[b], y[b]+1] = b /\ board[x[b], y[b]+2] = b /\ board[x[b]+1, y[b]+2] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b]+1, y[b]] = b /\ board[x[b]+2, y[b]] = b /\ board[x[b]+2, y[b]-1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b]-1, y[b]] = b /\ board[x[b]-2, y[b]] = b /\ board[x[b]-2, y[b]+1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b], y[b]-1] = b /\ board[x[b], y[b]-2] = b /\ board[x[b]-1, y[b]-2] = b)
    elseif BlockType[b] = S then
        (board[x[b], y[b]] = b /\ board[x[b]+1, y[b]] = b /\ board[x[b]+1, y[b]+1] = b /\ board[x[b]+2, y[b]+1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b], y[b]-1] = b /\ board[x[b]+1, y[b]-1] = b /\ board[x[b]+1, y[b]-2] = b)
    else
        true % I1 is already constrained
    endif
);

% Blocks are not placed twice and do not appear more than 4 times in the board (4-sized blocks)
constraint forall(b in BLOCKS)(
    sum(i in 1..4, j in 1..4)(board[i,j] = b) = 4  % 4-sized blocks
);

% Needs ~15s to solve it (on my laptop)
solve satisfy;

% Show board (4x4 grid)
output ["Board: [" ++ "\n" ++
            " " ++ show(board[1,1]) ++ " " ++ show(board[2,1]) ++ " " ++ show(board[3,1]) ++ " " ++ show(board[4,1]) ++ "\n" ++
            " " ++ show(board[1,2]) ++ " " ++ show(board[2,2]) ++ " " ++ show(board[3,2]) ++ " " ++ show(board[4,2]) ++ "\n" ++
            " " ++ show(board[1,3]) ++ " " ++ show(board[2,3]) ++ " " ++ show(board[3,3]) ++ " " ++ show(board[4,3]) ++ "\n" ++
            " " ++ show(board[1,4]) ++ " " ++ show(board[2,4]) ++ " " ++ show(board[3,4]) ++ " " ++ show(board[4,4]) ++ "\n]" ++ "\n"];