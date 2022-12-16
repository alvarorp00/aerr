% Space of 4x3 to be filled with two L and one O figures:
%  - Rows: 1..3
%  - Columns: 1..4

% Board

int: x_lim = 4;
int: y_lim = 3;

set of int: COLUMNS = 1..x_lim;
set of int: ROWS = 1..y_lim;

% Figures

enum Figure = {L,O};
enum Block = {L1,L2,O1};

array[Block] of Figure: BlockType = [L,L,O];

% blocktype constraints

constraint BlockType[L1] = L;
constraint BlockType[L2] = L;
constraint BlockType[O1] = O;

% end

set of Block: BLOCKS = {L1,L2,O1};

array[ROWS, COLUMNS] of var BLOCKS: board;  % the idea is to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

array[Block] of var COLUMNS: x;  % x coordinate of the block
array[Block] of var ROWS: y;  % y coordinate of the block

% Constraints

constraint (board[y[L1], x[L1]] = L1 /\ board[y[L1], x[L1]+1] = L1 /\ board[y[L1], x[L1]+2] = L1 /\ board[y[L1]-1, x[L1]+2] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1]+1, x[L1]] = L1 /\ board[y[L1]+2, x[L1]] = L1 /\ board[y[L1]+2, x[L1]+1] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1]-1, x[L1]] = L1 /\ board[y[L1]-2, x[L1]] = L1 /\ board[y[L1]-2, x[L1]-1] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1], x[L1]-1] = L1 /\ board[y[L1], x[L1]-2] = L1 /\ board[y[L1]+1, x[L1]-2] = L1);

constraint (board[y[L2], x[L2]] = L2 /\ board[y[L2], x[L2]+1] = L2 /\ board[y[L2], x[L2]+2] = L2 /\ board[y[L2]-1, x[L2]+2] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2]+1, x[L2]] = L2 /\ board[y[L2]+2, x[L2]] = L2 /\ board[y[L2]+2, x[L2]+1] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2]-1, x[L2]] = L2 /\ board[y[L2]-2, x[L2]] = L2 /\ board[y[L2]-2, x[L2]-1] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2], x[L2]-1] = L2 /\ board[y[L2], x[L2]-2] = L2 /\ board[y[L2]+1, x[L2]-2] = L2);

constraint (board[y[O1], x[O1]] = O1 /\ board[y[O1], x[O1]+1] = O1 /\ board[y[O1]+1, x[O1]] = O1 /\ board[y[O1]+1, x[O1]+1] = O1);

% constraint forall(b in BLOCKS)(
%     if BlockType[b] = L then
%         (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b], x[b]+2] = b /\ board[y[b]-1, x[b]+2] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b]+1, x[b]] = b /\ board[y[b]+2, x[b]] = b /\ board[y[b]+2, x[b]+1] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b]-1, x[b]] = b /\ board[y[b]-2, x[b]] = b /\ board[y[b]-2, x[b]-1] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b], x[b]-1] = b /\ board[y[b], x[b]-2] = b /\ board[y[b]+1, x[b]-2] = b)
%     elseif BlockType[b] = O then
%         (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b]+1, x[b]] = b /\ board[y[b]+1, x[b]+1] = b)
%     else
%         true % should never happen
%     endif
% );

% Blocks do not overlap

constraint forall(b in BLOCKS)(
    forall(i in ROWS, j in COLUMNS)(
        if board[i,j] = b then
            forall(k in ROWS, l in COLUMNS)(
                if k = i /\ l = j then
                    board[k,l] = b
                endif
            )
        endif
    )
);

% ann: search_ann;
% search_ann = int_search(board, first_fail, indomain_median, complete);
% solve :: search_ann satisfy;

solve satisfy;

% Show board (4x3 grid)
output ["Board: [" ++ "\n" ++
             " " ++ show(board[1,1]) ++ " " ++  show(board[1,2]) ++ " " ++  show(board[1,3]) ++ " " ++  show(board[1,4]) ++ "\n"
            ++ " " ++  show(board[2,1]) ++ " " ++  show(board[2,2]) ++ " " ++  show(board[2,3]) ++ " " ++ show(board[2,4]) ++ "\n"
            ++ " " ++  show(board[3,1]) ++ " " ++  show(board[3,2]) ++ " " ++  show(board[3,3]) ++ " " ++  show(board[3,4]) ++ "\n]" ++ "\n" ++ "\n"];