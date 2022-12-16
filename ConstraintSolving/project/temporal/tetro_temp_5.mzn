% Figures Lx2, Ix1, Ox1  --> should be satisfiable

% Space of 4x3 to be filled with two L and one O figures

% Board

int: x_lim = 4;
int: y_lim = 4;

set of int: COLUMNS = 1..x_lim;
set of int: ROWS = 1..y_lim;

% Figures

enum Figure = {L,O,I};
enum Block = {L1,L2,O1,I1};

array[Block] of Figure: BlockType = [L,L,O,I];

% blocktype constraints

constraint BlockType[L1] = L;
constraint BlockType[L2] = L;
constraint BlockType[O1] = O;
constraint BlockType[I1] = I;

% end

% set of Block: BLOCKS = {L1,L2,S1,I1};
set of Block: BLOCKS = {O1,I1,L1,L2};

array[ROWS, COLUMNS] of var BLOCKS: board;  % the idea is to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

array[Block] of var COLUMNS: x;  % x coordinate of the block
array[Block] of var ROWS: y;  % y coordinate of the block

% Constraints

constraint forall(b in BLOCKS)(
    if BlockType[b] = L then
        (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b], x[b]+2] = b /\ board[y[b]-1, x[b]+2] = b) \/
        (board[y[b], x[b]] = b /\ board[y[b]+1, x[b]] = b /\ board[y[b]+2, x[b]] = b /\ board[y[b]+2, x[b]+1] = b) \/
        (board[y[b], x[b]] = b /\ board[y[b]-1, x[b]] = b /\ board[y[b]-2, x[b]] = b /\ board[y[b]-2, x[b]-1] = b) \/
        (board[y[b], x[b]] = b /\ board[y[b], x[b]-1] = b /\ board[y[b], x[b]-2] = b /\ board[y[b]+1, x[b]-2] = b)
    elseif BlockType[b] = O then
        (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b]+1, x[b]] = b /\ board[y[b]+1, x[b]+1] = b)
    elseif BlockType[b] = I then
        (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b], x[b]+2] = b /\ board[y[b], x[b]+3] = b) \/
        (board[y[b], x[b]] = b /\ board[y[b]+1, x[b]] = b /\ board[y[b]+2, x[b]] = b /\ board[y[b]+3, x[b]] = b)
    else
        true % should never happen
    endif
);

% Blocks are not placed twice
constraint forall(b in BLOCKS)(
    sum(i in 1..4, j in 1..4)(board[i,j] = b) = 4  % 4-sized blocks so we can't have more than 4 cells occupied by the same block
);

% Solve
solve satisfy;

% Show board (4x4 grid)
output ["Board: [" ++ "\n" ++
             " " ++ show(board[1,1]) ++ " " ++  show(board[1,2]) ++ " " ++  show(board[1,3]) ++ " " ++  show(board[1,4]) ++ "\n"
            ++ " " ++  show(board[2,1]) ++ " " ++  show(board[2,2]) ++ " " ++  show(board[2,3]) ++ " " ++ show(board[2,4]) ++ "\n"
            ++ " " ++  show(board[3,1]) ++ " " ++  show(board[3,2]) ++ " " ++  show(board[3,3]) ++ " " ++ show(board[3,4]) ++ "\n"
            ++ " " ++  show(board[4,1]) ++ " " ++  show(board[4,2]) ++ " " ++  show(board[4,3]) ++ " " ++ show(board[4,4]) ++ "\n" ++ "];"];